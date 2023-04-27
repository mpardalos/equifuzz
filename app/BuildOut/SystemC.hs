{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module BuildOut.SystemC where

import BuildOut.Internal
import Data.Text (Text)
import Hedgehog (MonadGen)
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

newInput :: Int -> BuildOutM s Text
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

someWidth :: MonadGen m => m Int
someWidth = Hog.int (Hog.Range.constant 1 64)

someType :: MonadGen m => m SC.SCType
someType =
  Hog.choice
    [ SC.SCInt <$> someWidth,
      SC.SCUInt <$> someWidth,
      do
        w <- someWidth
        i <- Hog.int (Hog.Range.constant 1 w)
        return (SC.SCFixed {w, i}),
      do
        w <- someWidth
        i <- Hog.int (Hog.Range.constant 1 w)
        return (SC.SCUFixed {w, i}),
      pure SC.CUInt,
      pure SC.CInt
    ]

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
