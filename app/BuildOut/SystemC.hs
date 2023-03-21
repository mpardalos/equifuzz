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

deadExpression :: BuildOutM SC.Expr
deadExpression = SC.Constant <$> Hog.int (Hog.Range.constant (-255) 255)

ifTrue :: SC.Expr -> BuildOutM SC.Expr
ifTrue e =
  SC.Conditional
    <$> (SC.Constant <$> Hog.int (Hog.Range.constant 1 255))
    <*> pure e
    <*> deadExpression

ifFalse :: SC.Expr -> BuildOutM SC.Expr
ifFalse e =
  SC.Conditional
    (SC.Constant 0)
    <$> deadExpression
    <*> pure e

or0 :: SC.Expr -> BuildOutM SC.Expr
or0 e = pure (SC.BinOp e SC.BitwiseOr (SC.Constant 0))

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
