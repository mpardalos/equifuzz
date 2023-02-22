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

-- | Grow both expressions in a pair in an equivalent (but possibly not
-- identical) way
grow :: ExprPair -> Gen ExprPair
grow pair = do
  count <- Hog.int (Hog.Range.linear 1 20)
  iterateM count grow1 pair
  where
    grow1 p =
      Hog.choice
        [ bimapF ifFalse p,
          bimapF ifTrue p,
          bimapF or0 p
        ]

deadExpression :: Gen (Expr ())
deadExpression = Number () . fromIntegral <$> Hog.int (Hog.Range.constant 1 255)

bimapF :: Applicative f => (a -> f a) -> (a, a) -> f (a, a)
bimapF f (x, y) = (,) <$> f x <*> f y

ifTrue :: Expr () -> Gen (Expr ())
ifTrue e = do
  condition <- Number () . fromIntegral <$> Hog.int (Hog.Range.constant 1 255)
  falseBranch <- deadExpression
  return (Cond () condition e falseBranch)

ifFalse :: Expr () -> Gen (Expr ())
ifFalse e = do
  falseBranch <- deadExpression
  return (Cond () (Number () 0) e falseBranch)

or0 :: Expr () -> Gen (Expr ())
or0 e = pure (BinOp () e BinOr (Number () 0))

singleExprModule :: Identifier -> Expr () -> ModDecl ()
singleExprModule name e =
  ModDecl
    { annotation = (),
      params = [],
      inPorts = [],
      id = name,
      outPorts = [outPort],
      items =
        [ Decl () (Just PortOut) outPort Nothing,
          ModCA () (ContAssign "y" e)
        ]
    }
  where
    outPort =
      Port
        { portType = Wire,
          signed = False,
          size = Range (ConstNum () 7) (ConstNum () 0),
          name = "y"
        }

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ x = pure x
iterateM 1 f x = f x
iterateM n f x = do
  x' <- f x
  iterateM (n - 1) f x'
