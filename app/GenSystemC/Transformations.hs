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
import Optics.State.Operators ((%=), (.=))
import SystemC qualified as SC

data Transformation
  = CastWithDeclaration SC.SCType
  | Range Int Int
  | Arithmetic SC.BinOp (SC.Expr BuildOut)
  | UseAsCondition (SC.Expr BuildOut) (SC.Expr BuildOut)
  | BitSelect Int
  deriving stock (Show, Generic)

applyTransformation :: MonadBuildOut m => Transformation -> m ()
applyTransformation (CastWithDeclaration varType) = do
  e <- use #headExpr
  varName <- newVar
  #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])
  #headExpr .= SC.Variable varType varName
applyTransformation (Range hi lo) = do
  e <- use #headExpr
  case SC.rangeType e.annotation hi lo of
    Just subrefType ->
      #headExpr .= SC.Range subrefType e hi lo
    Nothing ->
      pure ()
applyTransformation (Arithmetic op e') = do
  e <- use #headExpr
  #headExpr .= SC.BinOp e'.annotation e op e'
applyTransformation (UseAsCondition tExpr fExpr) = do
  e <- use #headExpr
  #headExpr .= SC.Conditional tExpr.annotation e tExpr fExpr
applyTransformation (BitSelect idx) = do
  e <- use #headExpr
  case SC.supportsBitref e.annotation of
    Just bitrefType ->
      #headExpr .= SC.Bitref bitrefType e idx
    Nothing ->
      pure ()

data BuildOut

instance SC.Annotation BuildOut where
  type AnnExpr BuildOut = SC.SCType
  type AnnStatement BuildOut = ()

type MonadBuildOut m = MonadState BuildOutState m

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

newVar :: MonadBuildOut m => m Text
newVar = do
  varIdx <- use #nextVarIdx
  #nextVarIdx %= (+ 1)
  return ("x" <> T.pack (show varIdx))
