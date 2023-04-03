{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module BuildOut.SystemC where

import BuildOut.Internal
import Data.Text (Text)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (over, view)
import SystemC qualified as SC

data BuildOut

instance SC.Annotation BuildOut where
  type AnnExpr BuildOut = SC.SCType
  type AnnStatement BuildOut = ()

cast :: (SC.AnnExpr ann ~ SC.SCType) => SC.SCType -> SC.Expr ann -> SC.Expr ann
cast t = SC.Cast t t

constant :: Int -> SC.Expr BuildOut
constant = SC.Constant SC.CInt

typeof :: (SC.Annotation ann, SC.AnnExpr ann ~ SC.SCType) => SC.Expr ann -> SC.SCType
typeof = view #annotation

newInput :: BuildOutState s => Int -> BuildOutM s Text
newInput size = do
  InputPort _ name <- newInputPort size
  return name

or0 :: SC.Expr BuildOut -> BuildOutM s (SC.Expr BuildOut)
or0 e = pure (SC.BinOp (typeof e) e SC.BitwiseOr (constant 0))

-- (e + 0)
plus0 :: SC.Expr BuildOut -> BuildOutM s (SC.Expr BuildOut)
plus0 e = pure (SC.BinOp (typeof e) e SC.Plus (constant 0))

-- (e * 1)
times1 :: SC.Expr BuildOut -> BuildOutM s (SC.Expr BuildOut)
times1 e = pure (SC.BinOp (typeof e) e SC.Multiply (constant 1))

-- ((e - 1) + 1)
plusNMinusN :: SC.Expr BuildOut -> BuildOutM s (SC.Expr BuildOut)
plusNMinusN e = do
  n <- cast (typeof e) . constant <$> Hog.int (Hog.Range.constant 0 255)
  -- TODO: Check that this is as expected.
  let t1 = over SC.width (+ 1) (typeof e)
  let t2 = over SC.width (+ 1) t1
  pure (SC.BinOp t2 (SC.BinOp t1 e SC.Plus n) SC.Minus n)

ifTrue :: SC.Expr BuildOut -> BuildOutM s (SC.Expr BuildOut)
ifTrue e = do
  let cond = SC.Constant (SC.SCUInt 1) 1
  fBranch <- deadExpression (typeof e)
  pure (SC.Conditional (typeof e) cond e fBranch)

deadExpression :: SC.SCType -> BuildOutM s (SC.Expr BuildOut)
deadExpression t =
  case t of
    SC.SCUInt n -> cast t . constant <$> Hog.int (Hog.Range.constant 0 (2 ^ n - 1))
    SC.SCInt n -> cast t . constant <$> Hog.int (Hog.Range.constant (-(2 ^ (n - 1))) (2 ^ (n - 1) - 1))
    SC.CUInt -> constant <$> Hog.int (Hog.Range.constant 0 (2 ^ 32 - 1))
    SC.CInt -> constant <$> Hog.int (Hog.Range.constant (-(2 ^ (32 - 1))) (2 ^ (32 - 1) - 1))

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
