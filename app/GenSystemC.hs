{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module GenSystemC (GenConfig (..), genSystemCConstant) where

import Control.Applicative (Alternative)
import Control.Monad.State (MonadState (..), StateT (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hedgehog (Gen, MonadGen)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (use)
import Optics.State.Operators ((%=))
import SystemC qualified as SC
import Text.Printf (printf)
import Util (iterateM)

newtype GenConfig = GenConfig
  { growSteps :: Int
  }

genSystemCConstant :: GenConfig -> Text -> Gen (SC.FunctionDeclaration BuildOut)
genSystemCConstant config name = do
  (expr, finalState) <- runBuildOutM (genExpr config)
  return $
    SC.FunctionDeclaration
      { returnType = expr.annotation,
        name,
        args = [],
        body = finalState.statements ++ [SC.Return () expr]
      }

genExpr :: GenConfig -> BuildOutM (SC.Expr BuildOut)
genExpr config = seedExpr >>= grow config

seedExpr :: BuildOutM (SC.Expr BuildOut)
seedExpr = constant <$> Hog.int (Hog.Range.constant (-128) 128)

grow :: GenConfig -> SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
grow config scExpr = do
  grownScExpr <-
    iterateM
      config.growSteps
      (\e -> do t <- Hog.element transformations; t.apply e)
      scExpr
  if isFinalType grownScExpr.annotation
    then return grownScExpr
    else castToFinalType.apply grownScExpr
  where
    transformations :: [Transformation SC.Expr]
    transformations =
      [ castWithDeclaration,
        range,
        arithmetic,
        useAsCondition,
        bitSelect
      ]

newtype BuildOutM a = BuildOutM (StateT SCConstState Gen a)
  deriving newtype (Applicative, Monad, Alternative, MonadState SCConstState, MonadGen)
  deriving stock (Functor)

runBuildOutM :: BuildOutM a -> Gen (a, SCConstState)
runBuildOutM (BuildOutM m) =
  runStateT
    m
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

constant :: Int -> SC.Expr BuildOut
constant = SC.Constant SC.CInt

someWidth :: MonadGen m => m Int
someWidth = Hog.int (Hog.Range.constant 1 64)

data Transformation e = Transformation
  { name :: Text,
    apply :: e BuildOut -> BuildOutM (e BuildOut)
  }

instance Show (Transformation e) where
  show Transformation {name} =
    printf "Transformation { name = \"%s\", apply = ... }" (T.unpack name)

castWithDeclaration :: Transformation SC.Expr
castWithDeclaration = Transformation "castWithDeclaration" $ \e -> do
  varType <- castToType e.annotation
  varIdx <- use #nextVarIdx
  let varName = "x" <> T.pack (show varIdx)
  #nextVarIdx %= (+ 1)
  #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])
  return (SC.Variable varType varName)

castToFinalType :: Transformation SC.Expr
castToFinalType = Transformation "castToFinalType" $ \e -> do
  -- TODO Change this to specify the actually allowed "final" types. This works
  -- for now because all of the types in `castToType` are allowed as final
  varType <- castToType e.annotation
  varIdx <- use #nextVarIdx
  let varName = "x" <> T.pack (show varIdx)
  #nextVarIdx %= (+ 1)
  #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])
  return (SC.Variable varType varName)

useAsCondition :: Transformation SC.Expr
useAsCondition = Transformation "useAsCondition" $ \e ->
  if SC.CBool `elem` SC.implicitCastTargetsOf e.annotation
    then SC.Conditional SC.CInt e <$> (constant <$> someValue) <*> (constant <$> someValue)
    else pure e
  where
    someValue = Hog.int (Hog.Range.constant (-1024) 1024)

arithmetic :: Transformation SC.Expr
arithmetic = Transformation "arithmetic" $ \e ->
  let someOp =
        Hog.element
          [ SC.Plus,
            SC.Minus,
            SC.Multiply
          ]

      someConstant =
        SC.Constant resultType
          <$> Hog.int (Hog.Range.constant (-1024) 1024)

      resultType = case (SC.isIntegral e.annotation, SC.isSigned e.annotation) of
        (False, _) -> SC.CDouble
        (True, False) -> SC.CUInt
        (True, True) -> SC.CInt
   in if length (Set.intersection [SC.CInt, SC.CUInt, SC.CDouble] (SC.implicitCastTargetsOf e.annotation)) == 1
        then SC.BinOp resultType e <$> someOp <*> someConstant
        else pure e

-- | Generate a type that the input type can be cast to
castToType :: MonadGen m => SC.SCType -> m SC.SCType
castToType = \case
  -- FIXME: fixed-to-uint is broken for the version of vcf I am currently
  -- testing. This workaround should be an option at the top level.
  SC.SCFixed {} -> Hog.choice [someFixed, someUFixed]
  SC.SCUFixed {} -> Hog.choice [someFixed, someUFixed]
  SC.SCFxnumSubref {} -> Hog.choice [someInt, someUInt]
  _ -> Hog.choice [someInt, someUInt, someFixed, someUFixed]
  where
    someInt = SC.SCInt <$> someWidth
    someUInt = SC.SCUInt <$> someWidth
    someFixed = do
      w <- someWidth
      i <- Hog.int (Hog.Range.constant 0 w)
      return (SC.SCFixed w i)
    someUFixed = do
      w <- someWidth
      i <- Hog.int (Hog.Range.constant 0 w)
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

range :: Transformation SC.Expr
range = Transformation "range" $ \e ->
  case (SC.specifiedWidth e.annotation, SC.supportsRange e.annotation) of
    (Just width, Just subrefType) -> do
      hi <- Hog.int (Hog.Range.constant 0 (width - 1))
      lo <- Hog.int (Hog.Range.constant 0 hi)
      return (SC.Range subrefType e hi lo)
    _ -> return e

bitSelect :: Transformation SC.Expr
bitSelect = Transformation "bitSelect" $ \e ->
  case (SC.specifiedWidth e.annotation, SC.supportsBitref e.annotation) of
    (Just width, Just bitrefType) -> do
      bit <- Hog.int (Hog.Range.constant 0 (width - 1))
      return (SC.Bitref bitrefType e bit)
    _ -> return e
