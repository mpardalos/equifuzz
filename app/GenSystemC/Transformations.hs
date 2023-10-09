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
-- Import modOperations cfg as SCUnconfigured.operations, because we need to make
-- sure to run its result through GenConfig.modOperations, and never use it
-- directly
import SystemC qualified as SC hiding (operations)
import SystemC qualified as SCUnconfigured (operations)
import Data.List (intersect)
import GenSystemC.Config (GenConfig(..), GenMods(..), TransformationFlags(..))

data Transformation
  = CastWithAssignment SC.SCType
  | FunctionalCast SC.SCType
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

applyTransformation :: MonadBuild m => GenConfig -> Transformation -> m ()
applyTransformation _ (CastWithAssignment varType) = do
  e <- use #headExpr
  varName <- newVar
  #statements
    %= ( ++
           [ SC.Declaration () varType varName,
             SC.Assignment () varName e
           ]
       )
  #headExpr .= SC.Variable varType varName
applyTransformation _ (FunctionalCast castType) =
  #headExpr %= \e -> SC.Cast castType castType e
applyTransformation cfg (Range hi lo) =
  #headExpr %= \e -> case (modOperations cfg e.annotation).partSelect of
    Just subrefType | hi >= lo ->
      SC.MethodCall
        (subrefType (hi - lo + 1))
        e
        "range"
        [SC.Constant SC.CInt hi, SC.Constant SC.CInt lo]
    _ -> e
applyTransformation _ (Arithmetic op e') =
  #headExpr %= \e ->
    SC.BinOp e'.annotation e op e'
applyTransformation _ (UseAsCondition tExpr fExpr) = do
  #headExpr %= \e ->
    SC.Conditional tExpr.annotation e tExpr fExpr
applyTransformation cfg (BitSelect idx) = do
  #headExpr %= \e -> case (modOperations cfg e.annotation).bitSelect of
    Just bitrefType -> SC.Bitref bitrefType e idx
    Nothing -> e
applyTransformation cfg (ApplyReduction op) = do
  #headExpr %= \e ->
    if op `elem` (modOperations cfg e.annotation).reductions
      then SC.MethodCall SC.CBool e (SC.reductionMethod op) []
      else e

randomTransformationFor :: forall m. MonadRandom m => GenConfig -> SC.Expr BuildOut -> m Transformation
randomTransformationFor cfg e =
  join . weighted . map (,1) . catMaybes $
    [ guard cfg.mods.transformations.castWithAssignment >> castWithAssignment
    , guard cfg.mods.transformations.functionalCast >> functionalCast
    , guard cfg.mods.transformations.range >> range
#ifndef EVALUATION_VERSION
    , guard cfg.mods.transformations.arithmetic >> arithmetic
    , guard cfg.mods.transformations.useAsCondition >> useAsCondition
    , guard cfg.mods.transformations.bitSelect >> bitSelect
    , guard cfg.mods.transformations.applyReduction >> applyReduction
#endif
    ]
  where
    castWithAssignment :: Maybe (m Transformation)
    castWithAssignment = fmap CastWithAssignment <$> assignmentCastTargetType

    functionalCast :: Maybe (m Transformation)
    functionalCast = fmap FunctionalCast <$> functionalCastTargetType

    someWidth :: m Int
    someWidth = getRandomR (1, 64)

    someBigWidth :: m Int
    someBigWidth = getRandomR (1, 512)

    someWI :: m (Int, Int)
    someWI = do
      w <- someWidth
      i <- getRandomR (0, w)
      return (w, i)

    assignmentCastTargetType :: Maybe (m SC.SCType)
    assignmentCastTargetType =
      let ts = (modOperations cfg e.annotation).assignTo
          gens :: [m SC.SCType]
          gens = catMaybes
            [ guard ts.scInt >> Just (SC.SCInt <$> someWidth)
            , guard ts.scInt >> Just (SC.SCInt <$> someWidth)
            , guard ts.scUInt >> Just (SC.SCUInt <$> someWidth)
            , guard ts.scBigInt >> Just (SC.SCBigInt <$> someBigWidth)
            , guard ts.scBigUInt >> Just (SC.SCBigUInt <$> someBigWidth)
            , guard ts.scFixed >> Just (uncurry SC.SCFixed <$> someWI )
            , guard ts.scUFixed >> Just (uncurry SC.SCUFixed <$> someWI )
            -- No subrefs or bitrefs because they cannot be constructed
            , guard ts.cUInt >> Just (pure SC.CUInt)
            , guard ts.cInt >> Just (pure SC.CInt)
            , guard ts.cDouble >> Just (pure SC.CDouble)
            , guard ts.cBool >> Just (pure SC.CBool)
            ]

       in if null gens
             then Nothing
             else Just $ join (uniform gens)

    functionalCastTargetType :: Maybe (m SC.SCType)
    functionalCastTargetType =
      let cs = (modOperations cfg e.annotation).constructorInto
          gens :: [m SC.SCType]
          gens = catMaybes
            [ guard cs.scInt >> Just (SC.SCInt <$> someWidth)
            , guard cs.scInt >> Just (SC.SCInt <$> someWidth)
            , guard cs.scUInt >> Just (SC.SCUInt <$> someWidth)
            , guard cs.scBigInt >> Just (SC.SCBigInt <$> someBigWidth)
            , guard cs.scBigUInt >> Just (SC.SCBigUInt <$> someBigWidth)
            , guard cs.scFixed >> Just (uncurry SC.SCFixed <$> someWI )
            , guard cs.scUFixed >> Just (uncurry SC.SCUFixed <$> someWI )
            -- No subrefs or bitrefs because they cannot be constructed
            , guard cs.cUInt >> Just (pure SC.CUInt)
            , guard cs.cInt >> Just (pure SC.CInt)
            , guard cs.cDouble >> Just (pure SC.CDouble)
            , guard cs.cBool >> Just (pure SC.CBool)
            ]

       in if null gens
             then Nothing
             else Just $ join (uniform gens)

    range :: Maybe (m Transformation)
    range = do
      guard (isJust (modOperations cfg e.annotation).partSelect)
      exprWidth <- SC.knownWidth e.annotation
      return $ do
        hi <- getRandomR (0, exprWidth - 1)
        lo <- getRandomR (0, hi)
        return (Range hi lo)

    arithmeticResultType :: Maybe SC.SCType
    arithmeticResultType
      | [t] <-
          [SC.CInt, SC.CUInt, SC.CDouble]
            `intersect` (modOperations cfg e.annotation).implicitCasts =
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
      guard (SC.CBool `elem` (modOperations cfg e.annotation).implicitCasts)
      return
        ( UseAsCondition
            <$> someConstant SC.CInt
            <*> someConstant SC.CInt
        )

    bitSelect :: Maybe (m Transformation)
    bitSelect = do
      guard (isJust (modOperations cfg e.annotation).bitSelect)
      width <- SC.knownWidth e.annotation
      return (BitSelect <$> getRandomR (0, width - 1))

    applyReduction :: Maybe (m Transformation)
    applyReduction = do
      let options = ApplyReduction <$> (modOperations cfg e.annotation).reductions
      guard (not . null $ options)
      return (uniform options)

    someConstant :: SC.SCType -> m (SC.Expr BuildOut)
    someConstant t = SC.Constant t <$> getRandomR (-1024, 1024)

seedExpr :: MonadRandom m => m (SC.Expr BuildOut)
seedExpr = do
  value <- getRandomR (-128, 128)
  return (SC.Constant SC.CInt value)

modOperations :: GenConfig -> SC.SCType -> SC.Operations
modOperations cfg t = cfg.mods.operations t (SCUnconfigured.operations t)
