{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module GenSystemC (GenConfig (..), TransformationLabel, genSystemCConstant) where

import Control.Monad.Random.Strict (MonadRandom, Rand, StdGen, getRandomR, uniform)
import Control.Monad.State (MonadState (..), StateT (..))
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics (use)
import Optics.State.Operators ((%=))
import SystemC qualified as SC
import Util (iterateM)
import Control.Monad (join)

newtype GenConfig = GenConfig
  { growSteps :: Int
  }

type TransformationLabel = Text

genSystemCConstant :: GenConfig -> Text -> Rand StdGen ([TransformationLabel], SC.FunctionDeclaration BuildOut)
genSystemCConstant config name = do
  ((expr, transformations), finalState) <- runBuildOutM (genExpr config)
  return
    ( transformations,
      SC.FunctionDeclaration
        { returnType = expr.annotation,
          name,
          args = [],
          body = finalState.statements ++ [SC.Return () expr]
        }
    )

genExpr :: GenConfig -> BuildOutM (SC.Expr BuildOut)
genExpr config = seedExpr >>= grow config

seedExpr :: BuildOutM (SC.Expr BuildOut)
seedExpr = do
  value <- getRandomR (-128, 128)
  tell ["seedExpr(" <> T.pack (show value) <> ")"]
  return (SC.Constant SC.CInt value)

grow :: GenConfig -> SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
grow config scExpr = do
  steps <- getRandomR (max 10 config.growSteps, config.growSteps)
  grownScExpr <-
    iterateM
      steps
      ( \e -> do
          t <- uniform transformations
          t e
      )
      scExpr

  if isFinalType grownScExpr.annotation
    then return grownScExpr
    else castToFinalType grownScExpr
  where
    transformations :: [Transformation]
    transformations =
      [ castWithDeclaration
      , range
#ifndef EVALUATION_VERSION
      , arithmetic
      , useAsCondition
      , bitSelect
#endif
      ]

newtype BuildOutM a = BuildOutM (WriterT [TransformationLabel] (StateT SCConstState (Rand StdGen)) a)
  deriving newtype (Applicative, Monad, MonadWriter [TransformationLabel], MonadState SCConstState, MonadRandom)
  deriving stock (Functor)

runBuildOutM :: BuildOutM a -> Rand StdGen ((a, [TransformationLabel]), SCConstState)
runBuildOutM (BuildOutM m) =
  runStateT
    (runWriterT m)
    SCConstState
      { statements = [],
        nextVarIdx = 0
      }

data BuildOut

instance SC.Annotation BuildOut where
  type AnnExpr BuildOut = SC.SCType
  type AnnStatement BuildOut = ()

data SCConstState = SCConstState
  { statements :: [SC.Statement BuildOut],
    nextVarIdx :: Int
  }
  deriving (Generic)

-- | Transformations on SystemC expressions. They should also `tell` a TransformationLabel that they are applying
type Transformation = SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)

someConstant :: SC.SCType -> BuildOutM (SC.Expr BuildOut)
someConstant t = SC.Constant t <$> getRandomR (-1024, 1024)

someWidth :: MonadRandom m => m Int
someWidth = getRandomR (1, 64)

newVar :: BuildOutM Text
newVar = do
  varIdx <- use #nextVarIdx
  #nextVarIdx %= (+ 1)
  return ("x" <> T.pack (show varIdx))

castWithDeclaration :: Transformation
castWithDeclaration e = do
  varType <- castToType e.annotation

  varName <- newVar
  #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])

  tell ["castWithDeclaration(" <> SC.genSource varType <> ")"]
  return (SC.Variable varType varName)

castToFinalType :: Transformation
castToFinalType e = do
  -- TODO Change this to specify the actually allowed "final" types. This works
  -- for now because all of the types in `castToType` are allowed as final
  varType <- castToType e.annotation
  varIdx <- use #nextVarIdx
  let varName = "x" <> T.pack (show varIdx)
  #nextVarIdx %= (+ 1)
  #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])

  tell ["castToFinalType(" <> SC.genSource varType <> ")"]
  return (SC.Variable varType varName)

useAsCondition :: Transformation
useAsCondition e =
  if SC.CBool `elem` SC.implicitCastTargetsOf e.annotation
    then do
      trueValue <- someConstant SC.CInt
      falseValue <- someConstant SC.CInt
      tell ["useAsCondition(" <> SC.genSource trueValue <> ", " <> SC.genSource falseValue <> ")"]
      return (SC.Conditional SC.CInt e trueValue falseValue)
    else pure e

arithmetic :: Transformation
arithmetic e =
  let someOp =
        uniform
          [ SC.Plus,
            SC.Minus,
            SC.Multiply
          ]

      resultType = case (SC.isIntegral e.annotation, SC.isSigned e.annotation) of
        (False, _) -> SC.CDouble
        (True, False) -> SC.CUInt
        (True, True) -> SC.CInt
   in if length (Set.intersection (Set.fromList [SC.CInt, SC.CUInt, SC.CDouble]) (SC.implicitCastTargetsOf e.annotation)) == 1
        then do
          op <- someOp
          constant <- someConstant resultType
          tell ["arithmetic(" <> SC.genSource op <> " " <> SC.genSource constant <> ")"]
          return (SC.BinOp resultType e op constant)
        else pure e

-- | Generate a type that the input type can be cast to
castToType :: MonadRandom m => SC.SCType -> m SC.SCType
castToType = \case
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

range :: Transformation
range e =
  case (SC.specifiedWidth e.annotation, SC.supportsRange e.annotation) of
    (Just width, Just subrefType) -> do
      hi <- getRandomR (0, width - 1)
      lo <- getRandomR (0, hi)

      tell ["range(" <> T.pack (show hi) <> ", " <> T.pack (show lo) <> ")"]
      return (SC.Range subrefType e hi lo)
    _ -> return e

bitSelect :: Transformation
bitSelect e =
  case (SC.specifiedWidth e.annotation, SC.supportsBitref e.annotation) of
    (Just width, Just bitrefType) -> do
      bit <- getRandomR (0, width - 1)

      tell ["bitSelect(" <> T.pack (show bit) <> ")"]
      return (SC.Bitref bitrefType e bit)
    _ -> return e
