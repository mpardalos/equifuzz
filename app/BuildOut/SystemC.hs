{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildOut.SystemC where

import BuildOut.Internal
import Data.Text (Text)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (view, over)
import SystemC qualified as SC

data BuildOut

instance SC.Annotation BuildOut where
  type AnnExpr BuildOut = SC.SCType
  type AnnStatement BuildOut = ()

castConstant :: SC.SCType -> Int -> SC.Expr BuildOut
castConstant t val = SC.Cast t t (SC.Constant t val)

typeof :: (SC.Annotation ann, SC.AnnExpr ann ~ SC.SCType) => SC.Expr ann -> SC.SCType
typeof = view #annotation

newInput :: Int -> BuildOutM Text
newInput size = do
  InputPort _ name <- newInputPort size
  return name

or0 :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
or0 e = pure (SC.BinOp (typeof e) e SC.BitwiseOr (castConstant (typeof e) 0))

-- (e + 0)
plus0 :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
plus0 e = pure (SC.BinOp (typeof e) e SC.Plus (castConstant (typeof e) 0))

-- (e * 1)
times1 :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
times1 e = pure (SC.BinOp (typeof e) e SC.Multiply (castConstant (typeof e) 1))

-- ((e - 1) + 1)
plusNMinusN :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
plusNMinusN e = do
  n <- castConstant (typeof e) <$> Hog.int (Hog.Range.constant 0 255)
  -- TODO: Check that this is as expected.
  let t1 = over SC.width (+1) (typeof e)
  let t2 = over SC.width (+1) t1
  pure (SC.BinOp t2 (SC.BinOp t1 e SC.Plus n) SC.Minus n)

singleExprFunction ::
  SC.SCType ->
  Text ->
  [(SC.SCType, Text)] ->
  SC.Expr BuildOut ->
  SC.FunctionDeclaration BuildOut
singleExprFunction returnType name args e =
  SC.FunctionDeclaration
    { returnType,
      name,
      args,
      body = [SC.Return () e]
    }
