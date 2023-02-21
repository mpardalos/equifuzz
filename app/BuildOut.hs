{-# LANGUAGE DuplicateRecordFields #-}

module BuildOut where

import Control.Monad (guard)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Verismith.Verilog.AST

type ExprPair = (Expr (), Expr ())

inequivalent :: Gen ExprPair
inequivalent = Hog.choice [differentConstants]

differentConstants :: Gen ExprPair
differentConstants = do
  n1 <- Hog.int (Hog.Range.constant 0 255)
  n2 <- Hog.int (Hog.Range.constant 0 255)
  guard (n1 /= n2)
  return (Number () (fromIntegral n1), Number () (fromIntegral n2))

grow :: ExprPair -> Gen ExprPair
grow pair = do
  count <- Hog.int (Hog.Range.linear 1 20)
  iterateM count grow1 pair
  where
    grow1 (e1, e2) = do
      f <-
        Hog.choice
          [ ifTrue
          , ifFalse
          , or0
          ]
      return (f e1, f e2)

deadExpression :: Gen (Expr ())
deadExpression = Number () . fromIntegral <$> Hog.int (Hog.Range.constant 1 255)

ifTrue :: Gen (Expr () -> Expr ())
ifTrue = do
  condition <- Number () . fromIntegral <$> Hog.int (Hog.Range.constant 1 255)
  falseBranch <- deadExpression
  return (\e -> Cond () condition e falseBranch)

ifFalse :: Gen (Expr () -> Expr ())
ifFalse = do
  falseBranch <- deadExpression
  return (\e -> Cond () (Number () 0) e falseBranch)

or0 :: Gen (Expr () -> Expr ())
or0 = pure (\e -> BinOp () e BinOr (Number () 0))

singleExprModule :: Identifier -> Expr () -> ModDecl ()
singleExprModule name e =
  ModDecl
    { annotation = (),
      params = [],
      inPorts = [],
      id = name,
      outPorts =
        [ Port
            { portType = Wire,
              signed = False,
              size = Range (ConstNum () 7) (ConstNum () 0),
              name = "y"
            }
        ],
      items =
        [ ModCA () (ContAssign "y" e)
        ]
    }

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ x = pure x
iterateM 1 f x = f x
iterateM n f x = do
  x' <- f x
  iterateM (n - 1) f x'
