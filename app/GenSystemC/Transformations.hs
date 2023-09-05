{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenSystemC.Transformations where

import Control.Monad.State.Strict (MonadState)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics (use)
import Optics.State.Operators ((%=))
import SystemC qualified as SC

data Transformation
  = CastWithDeclaration SC.SCType
  | Range Int Int
  | Arithmetic SC.BinOp (SC.Expr BuildOut)
  | UseAsCondition (SC.Expr BuildOut) (SC.Expr BuildOut)
  | BitSelect Int
  deriving stock (Show, Generic)

applyTransformation :: MonadBuildOut m => Transformation -> SC.Expr BuildOut -> m (SC.Expr BuildOut)
applyTransformation (CastWithDeclaration varType) e = do
  varName <- newVar
  #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])
  return (SC.Variable varType varName)
applyTransformation (Range hi lo) e
  | Just subrefType <- SC.rangeType e.annotation hi lo =
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
