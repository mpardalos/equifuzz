{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenSystemC (GenConfig (..), Transformation (..), GenerateProcess (..), genSystemCConstant, generateFromProcess) where

import Control.Monad (foldM, guard, join)
import Control.Monad.Random.Strict (MonadRandom, Rand, StdGen, getRandomR, uniform, weighted)
import Control.Monad.State.Strict (MonadState, runState, runStateT)
import Control.Monad.Writer.Strict (MonadWriter, runWriterT, tell)
import Data.Maybe (catMaybes, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics (use)
import Optics.State.Operators ((%=))
import SystemC qualified as SC
import Util (iterateM)

newtype GenConfig = GenConfig
  { growSteps :: Int
  }

data Transformation
  = CastWithDeclaration SC.SCType
  | Range Int Int
  | Arithmetic SC.BinOp (SC.Expr BuildOut)
  | UseAsCondition (SC.Expr BuildOut) (SC.Expr BuildOut)
  | BitSelect Int
  deriving stock (Show, Generic)

data GenerateProcess = GenerateProcess
  { seed :: SC.Expr BuildOut,
    transformations :: [Transformation]
  }

generateFromProcess :: GenerateProcess -> Text -> SC.FunctionDeclaration BuildOut
generateFromProcess GenerateProcess {seed, transformations} name =
  let (expr, finalState) =
        foldM (flip applyTransformation) seed transformations
          `runState` initBuildOutState
   in SC.FunctionDeclaration
        { returnType = expr.annotation,
          name,
          args = [],
          body = finalState.statements ++ [SC.Return () expr]
        }

genSystemCConstant :: GenConfig -> Text -> Rand StdGen (GenerateProcess, SC.FunctionDeclaration BuildOut)
genSystemCConstant cfg name = do
  (seed, state1) <- runStateT seedExpr initBuildOutState

  ((expr, transformations), finalState) <-
    (`runStateT` state1) $ runWriterT (growExpr seed >>= finalizeExpr)

  return
    ( GenerateProcess seed transformations,
      SC.FunctionDeclaration
        { returnType = expr.annotation,
          name,
          args = [],
          body = finalState.statements ++ [SC.Return () expr]
        }
    )
  where
    growExpr ::
      (MonadBuildOut m, MonadRandom m, MonadWriter [Transformation] m) =>
      SC.Expr BuildOut ->
      m (SC.Expr BuildOut)
    growExpr = iterateM cfg.growSteps $ \e -> do
      transformation <- randomTransformationFor e
      tell [transformation]
      applyTransformation transformation e

    finalizeExpr ::
      (MonadBuildOut m, MonadRandom m, MonadWriter [Transformation] m) =>
      SC.Expr BuildOut ->
      m (SC.Expr BuildOut)
    finalizeExpr e
      | isFinalType e.annotation = return e
      | otherwise = do
          transformation <- finalCastTransformation e
          tell [transformation]
          applyTransformation transformation e

data BuildOut

instance SC.Annotation BuildOut where
  type AnnExpr BuildOut = SC.SCType
  type AnnStatement BuildOut = ()

type MonadBuildOut m = MonadState BuildOutState m

data BuildOutState = BuildOutState
  { statements :: [SC.Statement BuildOut],
    nextVarIdx :: Int
  }
  deriving (Generic)

initBuildOutState :: BuildOutState
initBuildOutState =
  BuildOutState
    { statements = [],
      nextVarIdx = 0
    }

newVar :: MonadBuildOut m => m Text
newVar = do
  varIdx <- use #nextVarIdx
  #nextVarIdx %= (+ 1)
  return ("x" <> T.pack (show varIdx))

applyTransformation :: MonadBuildOut m => Transformation -> SC.Expr BuildOut -> m (SC.Expr BuildOut)
applyTransformation (CastWithDeclaration varType) e = do
  varName <- newVar
  #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])
  return (SC.Variable varType varName)
applyTransformation (Range hi lo) e
  | Just subrefType <- SC.supportsRange e.annotation =
      return (SC.Range subrefType e hi lo)
  | otherwise =
      return e
applyTransformation (Arithmetic op e') e =
  return (SC.BinOp e'.annotation e op e')
applyTransformation (UseAsCondition tExpr fExpr) e =
  return (SC.Conditional tExpr.annotation e tExpr fExpr)
applyTransformation (BitSelect idx) e
  | Just bitrefType <- SC.supportsBitref e.annotation =
      return (SC.Bitref bitrefType e idx)
  | otherwise =
      return e

randomTransformationFor :: MonadRandom m => SC.Expr BuildOut -> m Transformation
randomTransformationFor e =
  join . weighted . map (,1) . catMaybes $
    [ Just castWithDeclaration,
      guard (isJust $ SC.supportsRange e.annotation)
        >> range <$> SC.specifiedWidth e.annotation,
      arithmetic <$> arithmeticResultType,
      guard canBeBool
        >> Just useAsCondition,
      guard (isJust $ SC.supportsBitref e.annotation)
        >> bitSelect <$> SC.specifiedWidth e.annotation
    ]
  where
    castWithDeclaration = CastWithDeclaration <$> castTargetType e.annotation

    range width = do
      hi <- getRandomR (0, width - 1)
      lo <- getRandomR (0, hi)
      return (Range hi lo)

    arithmeticResultType
      | [t] <-
          Set.toList $
            Set.intersection
              (Set.fromList [SC.CInt, SC.CUInt, SC.CDouble])
              (SC.implicitCastTargetsOf e.annotation) =
          Just t
      | otherwise = Nothing

    arithmetic resultType = do
      op <- uniform [SC.Plus, SC.Minus, SC.Multiply]
      constant <- someConstant resultType
      return (Arithmetic op constant)

    canBeBool :: Bool
    canBeBool = SC.CBool `elem` SC.implicitCastTargetsOf e.annotation

    useAsCondition =
      UseAsCondition
        <$> someConstant SC.CInt
        <*> someConstant SC.CInt

    bitSelect width = BitSelect <$> getRandomR (0, width - 1)

    someConstant :: MonadRandom m => SC.SCType -> m (SC.Expr BuildOut)
    someConstant t = SC.Constant t <$> getRandomR (-1024, 1024)

finalCastTransformation :: MonadRandom m => SC.Expr BuildOut -> m Transformation
finalCastTransformation e = CastWithDeclaration <$> finalCastType e.annotation

seedExpr :: MonadRandom m => m (SC.Expr BuildOut)
seedExpr = do
  value <- getRandomR (-128, 128)
  return (SC.Constant SC.CInt value)

isFinalType :: SC.SCType -> Bool
isFinalType SC.SCInt {} = True
isFinalType SC.SCFixed {} = True
isFinalType SC.SCUInt {} = True
isFinalType SC.SCUFixed {} = True
isFinalType SC.CInt = False
isFinalType SC.CUInt = False
isFinalType SC.CDouble = False
isFinalType SC.CBool = False
isFinalType SC.SCFxnumSubref = False
isFinalType SC.SCIntSubref = False
isFinalType SC.SCUIntSubref = False
isFinalType SC.SCIntBitref = False
isFinalType SC.SCUIntBitref = False

finalCastType :: MonadRandom m => SC.SCType -> m SC.SCType
finalCastType =
  -- It just so happens that all the types generated by `castTargetType` are
  -- valid final types. This alias exists to show that we specifically want
  -- this.
  castTargetType

-- Generate a type that the input type can be cast to
castTargetType :: MonadRandom m => SC.SCType -> m SC.SCType
castTargetType = \case
  -- FIXME: fixed-to-uint is broken for the version of vcf I am currently
  -- testing. This workaround should be an option at the top level.
  SC.SCFixed {} -> join $ uniform [someFixed, someUFixed]
  SC.SCUFixed {} -> join $ uniform [someFixed, someUFixed]
  SC.SCFxnumSubref {} -> join $ uniform [someInt, someUInt]
  _ -> join $ uniform [someInt, someUInt, someFixed, someUFixed]
  where
    someInt = SC.SCInt <$> someWidth
    someUInt = SC.SCUInt <$> someWidth
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
