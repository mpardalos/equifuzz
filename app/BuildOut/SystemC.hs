{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildOut.SystemC where

import BuildOut.Internal
import Data.Text (Text)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import SystemC qualified as SC

newInput :: Int -> BuildOutM Text
newInput size = do
  InputPort _ name <- newInputPort size
  return name

or0 :: SC.Expr -> BuildOutM SC.Expr
or0 e = pure (SC.BinOp e SC.BitwiseOr (SC.Constant 0))

-- ((e - 1) + 1)
plusNMinusN :: SC.Expr -> BuildOutM SC.Expr
plusNMinusN e = do
  n <- Hog.int (Hog.Range.constant 0 255)
  pure (SC.BinOp (SC.BinOp e SC.Plus (SC.Constant n)) SC.Minus (SC.Constant n))

singleExprFunction ::
  SC.SCType ->
  Text ->
  [(SC.SCType, Text)] ->
  SC.Expr ->
  SC.FunctionDeclaration
singleExprFunction returnType name args e =
  SC.FunctionDeclaration
    { returnType,
      name,
      args,
      body = [SC.Return e]
    }
