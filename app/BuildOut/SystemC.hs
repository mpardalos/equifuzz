{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildOut.SystemC where

import BuildOut.Internal
import Data.Data (Data)
import Data.Default.Class (Default (..))
import Data.Text (Text)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (view)
import Prettyprinter (Pretty (pretty))
import SystemC qualified as SC

data BuildOut

newtype Bitwidth = Bitwidth Int
  deriving stock (Data, Show)
  deriving newtype (Num, Eq, Ord)

instance Pretty Bitwidth where
  pretty (Bitwidth n) = pretty n <> "b"

instance Default Bitwidth where
  def = Bitwidth 0

instance SC.Annotation BuildOut where
  type AnnExpr BuildOut = Bitwidth
  type AnnStatement BuildOut = ()

boConstant :: Int -> SC.Expr BuildOut
boConstant n = SC.Constant (Bitwidth numBits) n
  where
    numBits = case n of
      0 -> 0
      1 -> 1
      _ -> ceiling $ logBase @Double 2.0 $ fromIntegral n

bitwidth :: (SC.Annotation ann, SC.AnnExpr ann ~ Bitwidth) => SC.Expr ann -> Bitwidth
bitwidth = view #annotation

newInput :: Int -> BuildOutM Text
newInput size = do
  InputPort _ name <- newInputPort size
  return name

or0 :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
or0 e = pure (SC.BinOp (bitwidth e) e SC.BitwiseOr (boConstant 0))

-- (e + 0)
plus0 :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
plus0 e = pure (SC.BinOp (bitwidth e) e SC.Plus (boConstant 0))

-- (e * 1)
times1 :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
times1 e = pure (SC.BinOp (bitwidth e) e SC.Multiply (boConstant 1))

-- ((e - 1) + 1)
plusNMinusN :: SC.Expr BuildOut -> BuildOutM (SC.Expr BuildOut)
plusNMinusN e = do
  n <- boConstant <$> Hog.int (Hog.Range.constant 0 255)
  -- TODO: Check that this is as expected.
  let width = max (bitwidth n) (bitwidth e) + 1
  pure (SC.BinOp width (SC.BinOp width e SC.Plus n) SC.Minus n)

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
