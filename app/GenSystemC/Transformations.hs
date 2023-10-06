{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module GenSystemC.Transformations
  ( -- * Transformations
    Transformation (..),
    BuildOut,

    -- * Applying transformations
    applyTransformation,

    -- ** Monad for applying transformations
    MonadBuild,
    BuildOutState (..),
    initBuildOutState,

    -- * Generating transformations
    randomTransformationFor,
    seedExpr,
  )
where

import Control.Monad (guard, join)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform, weighted)
import Control.Monad.State.Strict (MonadState)
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics (use)
import Optics.State.Operators ((%=), (.=))
import SystemC qualified as SC
import Data.List (intersect)

data Transformation
  = CastWithDeclaration SC.SCType
  | Range Int Int
  | Arithmetic SC.BinOp (SC.Expr BuildOut)
  | UseAsCondition (SC.Expr BuildOut) (SC.Expr BuildOut)
  | BitSelect Int
  | ApplyReduction SC.ReductionOperation
  deriving stock (Show, Generic)

data BuildOut

instance SC.Annotation BuildOut where
  type AnnExpr BuildOut = SC.SCType
  type AnnStatement BuildOut = ()

type MonadBuild m = MonadState BuildOutState m

data BuildOutState = BuildOutState
  { statements :: [SC.Statement BuildOut],
    headExpr :: SC.Expr BuildOut,
    nextVarIdx :: Int
  }
  deriving (Generic)

initBuildOutState :: SC.Expr BuildOut -> BuildOutState
initBuildOutState headExpr =
  BuildOutState
    { statements = [],
      headExpr,
      nextVarIdx = 0
    }

newVar :: MonadBuild m => m Text
newVar = do
  varIdx <- use #nextVarIdx
  #nextVarIdx %= (+ 1)
  return ("x" <> T.pack (show varIdx))

applyTransformation :: MonadBuild m => Transformation -> m ()
applyTransformation (CastWithDeclaration varType) = do
  e <- use #headExpr
  varName <- newVar
  #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])
  #headExpr .= SC.Variable varType varName
applyTransformation (Range hi lo) =
  #headExpr %= \e -> case (SC.operations e.annotation).partSelect of
    Just subrefType | hi >= lo ->
      SC.MethodCall
        (subrefType (hi - lo + 1))
        e
        "range"
        [SC.Constant SC.CInt hi, SC.Constant SC.CInt lo]
    _ -> e
applyTransformation (Arithmetic op e') =
  #headExpr %= \e ->
    SC.BinOp e'.annotation e op e'
applyTransformation (UseAsCondition tExpr fExpr) = do
  #headExpr %= \e ->
    SC.Conditional tExpr.annotation e tExpr fExpr
applyTransformation (BitSelect idx) = do
  #headExpr %= \e -> case (SC.operations e.annotation).bitSelect of
    Just bitrefType -> SC.Bitref bitrefType e idx
    Nothing -> e
applyTransformation (ApplyReduction op) = do
  #headExpr %= \e ->
    if op `elem` (SC.operations e.annotation).reductions
      then SC.MethodCall SC.CBool e (SC.reductionMethod op) []
      else e

randomTransformationFor :: forall m. MonadRandom m => SC.Expr BuildOut -> m Transformation
randomTransformationFor e =
  join . weighted . map (,1) . catMaybes $
    [ castWithDeclaration
    , range
#ifndef EVALUATION_VERSION
    , arithmetic
    , useAsCondition
    , bitSelect
    , applyReduction
#endif
    ]
  where
    castWithDeclaration :: Maybe (m Transformation)
    castWithDeclaration = Just (CastWithDeclaration <$> castTargetType)
      where
        castTargetType = case e.annotation of
          SC.SCFxnumSubref {} -> join $ uniform [someInt, someUInt]
          _ -> join $ uniform [someInt, someUInt, someBigInt, someBigUInt, someFixed, someUFixed]

        someInt = SC.SCInt <$> someWidth
        someUInt = SC.SCUInt <$> someWidth
        someBigInt = SC.SCBigInt <$> someBigWidth
        someBigUInt = SC.SCBigUInt <$> someBigWidth
        someFixed = do
          w <- someWidth
          i <- getRandomR (0, w)
          return (SC.SCFixed w i)
        someUFixed = do
          w <- someWidth
          i <- getRandomR (0, w)
          return (SC.SCUFixed w i)

        someWidth :: MonadRandom m => m Int
        someWidth = getRandomR (1, 64)

        someBigWidth :: MonadRandom m => m Int
        someBigWidth = getRandomR (1, 512)

    range :: Maybe (m Transformation)
    range = do
      guard (isJust (SC.operations e.annotation).partSelect)
      exprWidth <- SC.knownWidth e.annotation
      return $ do
        hi <- getRandomR (0, exprWidth - 1)
        lo <- getRandomR (0, hi)
        return (Range hi lo)

    arithmeticResultType :: Maybe SC.SCType
    arithmeticResultType
      | [t] <-
          [SC.CInt, SC.CUInt, SC.CDouble]
            `intersect` (SC.operations e.annotation).implicitCasts =
          Just t
      | otherwise = Nothing

    arithmetic :: Maybe (m Transformation)
    arithmetic = do
      resultType <- arithmeticResultType
      return $ do
        op <- uniform [SC.Plus, SC.Minus, SC.Multiply]
        constant <- someConstant resultType
        return (Arithmetic op constant)

    useAsCondition :: Maybe (m Transformation)
    useAsCondition = do
      guard (SC.CBool `elem` (SC.operations e.annotation).implicitCasts)
      return
        ( UseAsCondition
            <$> someConstant SC.CInt
            <*> someConstant SC.CInt
        )

    bitSelect :: Maybe (m Transformation)
    bitSelect = do
      guard (isJust (SC.operations e.annotation).bitSelect)
      width <- SC.knownWidth e.annotation
      return (BitSelect <$> getRandomR (0, width - 1))

    applyReduction :: Maybe (m Transformation)
    applyReduction = do
      let options = ApplyReduction <$> (SC.operations e.annotation).reductions
      guard (not . null $ options)
      return (uniform options)

    someConstant :: SC.SCType -> m (SC.Expr BuildOut)
    someConstant t = SC.Constant t <$> getRandomR (-1024, 1024)

seedExpr :: MonadRandom m => m (SC.Expr BuildOut)
seedExpr = do
  value <- getRandomR (-128, 128)
  return (SC.Constant SC.CInt value)
