{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenSystemC (genSystemCConstant) where

import Control.Applicative (Alternative)
import Control.Monad.State (MonadState (..), StateT (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hedgehog (Gen, MonadGen)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (use)
import Optics.State.Operators ((%=))
import SystemC as SC
import Util (iterateM)

genSystemCConstant :: Text -> Gen (SC.FunctionDeclaration BuildOut)
genSystemCConstant name = do
  (expr, finalState) <- runBuildOutM genExpr
  return $
    SC.FunctionDeclaration
      { returnType = expr.annotation,
        name,
        args = [],
        body = finalState.statements ++ [SC.Return () expr]
      }

genExpr :: BuildOutM (SC.Expr BuildOut)
genExpr = seedExpr >>= grow

seedExpr :: BuildOutM (SC.Expr BuildOut)
seedExpr = constant <$> Hog.int (Hog.Range.constant (-128) 128)

grow :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
grow scExpr = do
  -- count <- Hog.int (Hog.Range.linear 5 50)
  let count = 50
  grownScExpr <-
    iterateM
      count
      (\e -> do f <- Hog.element growFuncs; f e)
      scExpr
  if isFinalType grownScExpr.annotation
    then return grownScExpr
    else castToFinalType grownScExpr
  where
    growFuncs :: [SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)]
    growFuncs =
      [ castWithDeclaration,
        range,
        arithmetic
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

castWithDeclaration :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
castWithDeclaration e = do
  varType <- castToType e.annotation
  varIdx <- use #nextVarIdx
  let varName = "x" <> T.pack (show varIdx)
  #nextVarIdx %= (+ 1)
  #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])
  return (SC.Variable varType varName)

castToFinalType :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
castToFinalType e = do
  -- TODO Change this to specify the actually allowed "final" types. This works
  -- for now because all of the types in `castToType` are allowed as final
  varType <- castToType e.annotation
  varIdx <- use #nextVarIdx
  let varName = "x" <> T.pack (show varIdx)
  #nextVarIdx %= (+ 1)
  #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])
  return (SC.Variable varType varName)

arithmetic :: Expr BuildOut -> BuildOutM (Expr BuildOut)
arithmetic e
  | any (`elem` [SC.CInt, SC.CUInt, SC.CDouble]) (implicitCastTargetsOf e.annotation) =
      SC.BinOp resultType e
        <$> someOp
        <*> someConstant
  | otherwise = pure e
  where
    someOp =
      Hog.element
        [ SC.Plus,
          SC.Minus,
          SC.Multiply
        ]

    someConstant =
      SC.Constant resultType
        <$> Hog.int (Hog.Range.constant (-1024) 1024)

    resultType = case (isIntegral e.annotation, isSigned e.annotation) of
      (False, _) -> SC.CDouble
      (True, False) -> SC.CUInt
      (True, True) -> SC.CInt

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
isFinalType SC.SCFxnumSubref = False
isFinalType SC.SCIntSubref = False
isFinalType SC.SCUIntSubref = False

range :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
range e = case (SC.specifiedWidth e.annotation, SC.supportsRange e.annotation) of
  (Just width, Just subrefType) -> do
    hi <- Hog.int (Hog.Range.constant 0 (width - 1))
    lo <- Hog.int (Hog.Range.constant 0 hi)
    return (SC.Range subrefType e hi lo)
  _ -> return e
