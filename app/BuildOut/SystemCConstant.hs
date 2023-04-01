{-# OPTIONS -fdefer-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module BuildOut.SystemCConstant where

import BuildOut.Internal (BuildOutM)
import BuildOut.SystemC qualified as SC
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import SystemC qualified as SC

genExpr :: BuildOutM (SC.Expr SC.BuildOut)
genExpr = seedExpr >>= grow

seedExpr :: BuildOutM (SC.Expr SC.BuildOut)
seedExpr = SC.constant <$> Hog.int (Hog.Range.constant (-128) 128)

grow :: SC.Expr SC.BuildOut -> BuildOutM (SC.Expr SC.BuildOut)
grow e = Hog.choice []

randomBinOp :: SC.Expr SC.BuildOut -> BuildOutM (SC.Expr SC.BuildOut)
randomBinOp lhs = do
  op <-
    Hog.element
      [ SC.Plus,
        SC.Minus,
        SC.Multiply,
        SC.Divide,
        SC.BitwiseOr
      ]
  rhs <- SC.constant <$> Hog.int (Hog.Range.constant 0 512)
  return $ SC.BinOp (binOpResult lhs.annotation op rhs.annotation) lhs op rhs

binOpResult :: SC.SCType -> SC.BinOp -> SC.SCType -> SC.SCType
binOpResult lType op rType = case rType of
