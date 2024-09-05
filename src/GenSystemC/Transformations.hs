{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module GenSystemC.Transformations
  ( -- * Transformations
    Transformation (..),

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

import Control.Monad (guard, join, when)
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
import Util (is)
import qualified Data.Map as Map

data Transformation
  = CastWithAssignment SC.SCType
  | FunctionalCast SC.SCType
  | Range Int Int
  | Arithmetic SC.BinOp SC.Expr
  | UseAsCondition SC.Expr SC.Expr
  | BitSelect Int
  | ApplyMethod SC.SCMethod
  | ApplyUnaryOp SC.UnaryOp
  deriving stock (Show, Generic)


type MonadBuild m = MonadState BuildOutState m

data BuildOutState = BuildOutState
  { statements :: [SC.Statement],
    headExpr :: SC.Expr,
    nextVarIdx :: Int
  }
  deriving (Generic)

initBuildOutState :: SC.Expr -> BuildOutState
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

assignAllowed :: GenConfig -> SC.Expr -> SC.SCType -> Bool
assignAllowed cfg expr toType =
  let flags = (modOperations cfg expr).assignTo
   in case toType of
    SC.SCInt{} -> flags.scInt
    SC.SCUInt{} -> flags.scUInt
    SC.SCBigInt{} -> flags.scBigInt
    SC.SCBigUInt{} -> flags.scBigUInt
    SC.SCFixed{} -> flags.scFixed
    SC.SCUFixed{} -> flags.scUFixed
    SC.SCFxnumSubref{} -> flags.scFxnumSubref
    SC.SCIntSubref{} -> flags.scIntSubref
    SC.SCUIntSubref{} -> flags.scUIntSubref
    SC.SCSignedSubref{} -> flags.scSignedSubref
    SC.SCUnsignedSubref{} -> flags.scUnsignedSubref
    SC.SCIntBitref -> flags.scIntBitref
    SC.SCUIntBitref -> flags.scUIntBitref
    SC.SCSignedBitref -> flags.scSignedBitref
    SC.SCUnsignedBitref -> flags.scUnsignedBitref
    SC.SCLogic -> flags.scLogic
    SC.SCBV{} -> flags.scBV
    SC.SCLV{} -> flags.scLV
    SC.CUInt -> flags.cUInt
    SC.CInt -> flags.cInt
    SC.CDouble -> flags.cDouble
    SC.CBool -> flags.cBool

castAllowed :: GenConfig -> SC.Expr -> SC.SCType -> Bool
castAllowed cfg expr toType =
  let flags = (modOperations cfg expr).constructorInto
   in case toType of
    SC.SCInt{} -> flags.scInt
    SC.SCUInt{} -> flags.scUInt
    SC.SCBigInt{} -> flags.scBigInt
    SC.SCBigUInt{} -> flags.scBigUInt
    SC.SCFixed{} -> flags.scFixed
    SC.SCUFixed{} -> flags.scUFixed
    SC.SCFxnumSubref{} -> flags.scFxnumSubref
    SC.SCIntSubref{} -> flags.scIntSubref
    SC.SCUIntSubref{} -> flags.scUIntSubref
    SC.SCSignedSubref{} -> flags.scSignedSubref
    SC.SCUnsignedSubref{} -> flags.scUnsignedSubref
    SC.SCIntBitref -> flags.scIntBitref
    SC.SCUIntBitref -> flags.scUIntBitref
    SC.SCSignedBitref -> flags.scSignedBitref
    SC.SCUnsignedBitref -> flags.scUnsignedBitref
    SC.SCLogic -> flags.scLogic
    SC.SCBV{} -> flags.scBV
    SC.SCLV{} -> flags.scLV
    SC.CUInt -> flags.cUInt
    SC.CInt -> flags.cInt
    SC.CDouble -> flags.cDouble
    SC.CBool -> flags.cBool

applyTransformation :: MonadBuild m => GenConfig -> Transformation -> m ()
applyTransformation cfg (CastWithAssignment varType) = do
  e <- use #headExpr
  varName <- newVar
  when (assignAllowed cfg e varType) $ do
    #statements
      %= ( ++
            [ SC.Declaration varType varName,
              SC.Assignment varName e
            ]
        )
    #headExpr .= SC.Variable varType varName
applyTransformation cfg (FunctionalCast castType) =
  #headExpr %= \e ->
    if castAllowed cfg e castType
       then SC.Cast castType castType e
       else e
applyTransformation cfg (Range bound1 bound2) = do
  #headExpr %= \e ->
    let hi = max bound1 bound2
        lo = min bound1 bound2
        width = hi - lo + 1
        rangeInBounds = lo >= 0 && maybe True (hi <) (SC.knownWidth e.annotation)
     in case (modOperations cfg e).partSelect of
          Just resultType
            | rangeInBounds ->
                SC.MethodCall
                  (resultType width)
                  e
                  "range"
                  [SC.Constant SC.CInt hi, SC.Constant SC.CInt lo]
          _ -> e
applyTransformation _ (Arithmetic op e') =
  #headExpr %= \e ->
    SC.BinOp e'.annotation e op e'
applyTransformation cfg (UseAsCondition tExpr fExpr) = do
  #headExpr %= \e ->
    if SC.CBool `elem` (modOperations cfg e).implicitCasts
    then SC.Conditional tExpr.annotation e tExpr fExpr
    else e
applyTransformation cfg (BitSelect idx) = do
  #headExpr %= \e ->
    let idxInBounds = maybe True (idx <) (SC.knownWidth e.annotation)
     in case (modOperations cfg e).bitSelect of
          Just bitrefType | idxInBounds -> SC.Bitref bitrefType e idx
          _ -> e
applyTransformation cfg (ApplyMethod method) = do
  #headExpr %= \e ->
    case Map.lookup method (modOperations cfg e).methods of
      Just sig -> SC.MethodCall sig e (SC.methodName method) []
      Nothing -> e
applyTransformation cfg (ApplyUnaryOp op) = do
  #headExpr %= \e ->
    if (e `is` #_Variable) && (modOperations cfg e).incrementDecrement
      then SC.UnaryOp e.annotation op e
      else e

randomTransformationFor :: forall m. MonadRandom m => GenConfig -> SC.Expr -> m Transformation
randomTransformationFor cfg e
  | null transformationOptions = error ("No transformations possible on this expression: " <> show e)
  | otherwise = join . weighted . map (,1) $ transformationOptions
  where
    transformationOptions :: [m Transformation]
    transformationOptions = catMaybes
      [ guard cfg.mods.transformations.castWithAssignment >> castWithAssignment
      , guard cfg.mods.transformations.functionalCast >> functionalCast
      , guard cfg.mods.transformations.range >> range
#ifndef EVALUATION_VERSION
      , guard cfg.mods.transformations.arithmetic >> arithmetic
      , guard cfg.mods.transformations.useAsCondition >> useAsCondition
      , guard cfg.mods.transformations.bitSelect >> bitSelect
      , guard cfg.mods.transformations.applyMethod >> applyMethod
      , guard cfg.mods.transformations.applyUnaryOp >> applyUnaryOp
#endif
      ]
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
      let ts = (modOperations cfg e).assignTo
          gens :: [m SC.SCType]
          gens = catMaybes
            [ guard ts.scInt >> Just (SC.SCInt <$> someWidth)
            , guard ts.scInt >> Just (SC.SCInt <$> someWidth)
            , guard ts.scUInt >> Just (SC.SCUInt <$> someWidth)
            , guard ts.scBigInt >> Just (SC.SCBigInt <$> someBigWidth)
            , guard ts.scBigUInt >> Just (SC.SCBigUInt <$> someBigWidth)
            , guard ts.scFixed >> Just (uncurry SC.SCFixed <$> someWI )
            , guard ts.scUFixed >> Just (uncurry SC.SCUFixed <$> someWI )
            , guard ts.scLogic >> Just (pure SC.SCLogic)
            , guard ts.scBV >> Just (SC.SCBV <$> someWidth)
            , guard ts.scLV >> Just (SC.SCLV <$> someWidth)
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
      let cs = (modOperations cfg e).constructorInto
          gens :: [m SC.SCType]
          gens = catMaybes
            [ guard cs.scInt >> Just (SC.SCInt <$> someWidth)
            , guard cs.scInt >> Just (SC.SCInt <$> someWidth)
            , guard cs.scUInt >> Just (SC.SCUInt <$> someWidth)
            , guard cs.scBigInt >> Just (SC.SCBigInt <$> someBigWidth)
            , guard cs.scBigUInt >> Just (SC.SCBigUInt <$> someBigWidth)
            , guard cs.scFixed >> Just (uncurry SC.SCFixed <$> someWI )
            , guard cs.scUFixed >> Just (uncurry SC.SCUFixed <$> someWI )
            , guard cs.scLogic >> Just (pure SC.SCLogic)
            , guard cs.scBV >> Just (SC.SCBV <$> someWidth)
            , guard cs.scLV >> Just (SC.SCLV <$> someWidth)
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
      guard (isJust (modOperations cfg e).partSelect)
      exprWidth <- SC.knownWidth e.annotation
      return $ do
        hi <- getRandomR (0, exprWidth - 1)
        lo <- getRandomR (0, hi)
        return (Range hi lo)

    arithmeticResultType :: Maybe SC.SCType
    arithmeticResultType
      | [t] <-
          [SC.CInt, SC.CUInt, SC.CDouble]
            `intersect` (modOperations cfg e).implicitCasts =
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
      guard (SC.CBool `elem` (modOperations cfg e).implicitCasts)
      return
        ( UseAsCondition
            <$> someConstant SC.CInt
            <*> someConstant SC.CInt
        )

    bitSelect :: Maybe (m Transformation)
    bitSelect = do
      guard (isJust (modOperations cfg e).bitSelect)
      width <- SC.knownWidth e.annotation
      return (BitSelect <$> getRandomR (0, width - 1))

    applyMethod :: Maybe (m Transformation)
    applyMethod = do
      let options = ApplyMethod <$> Map.keys (modOperations cfg e).methods
      guard (not . null $ options)
      return (uniform options)

    applyUnaryOp :: Maybe (m Transformation)
    applyUnaryOp = do
      guard (modOperations cfg e).incrementDecrement
      guard (e `is` #_Variable)
      return (ApplyUnaryOp <$> uniform [minBound :: SC.UnaryOp .. maxBound])

    someConstant :: SC.SCType -> m SC.Expr
    someConstant t = SC.Constant t <$> getRandomR (-1024, 1024)

seedExpr :: MonadRandom m => m SC.Expr
seedExpr = do
  value <- getRandomR (-128, 128)
  return (SC.Constant SC.CInt value)

modOperations :: GenConfig -> SC.Expr -> SC.Operations
modOperations cfg e = cfg.mods.operations e (SCUnconfigured.operations e)
