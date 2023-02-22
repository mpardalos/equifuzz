{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module BuildOut where

import Control.Monad (guard)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Verismith.Verilog.AST

newtype ExprSource = ExprSource Text
  deriving newtype (IsString)
  deriving stock (Generic, Data)

instance Show ExprSource where
  show (ExprSource txt) = T.unpack txt

data BuildOut

instance Annotation BuildOut where
  type AnnExpr BuildOut = ExprSource
  type AnnConstExpr BuildOut = ()
  type AnnStatement BuildOut = ()
  type AnnModItem BuildOut = ()
  type AnnModDecl BuildOut = ()

instance Default ExprSource where
  def = ExprSource ""

type ExprPair = (Expr BuildOut, Expr BuildOut)

inequivalent :: Gen ExprPair
inequivalent = Hog.choice [differentConstants]

differentConstants :: Gen ExprPair
differentConstants = do
  n1 <- Hog.int (Hog.Range.constant 0 255)
  n2 <- Hog.int (Hog.Range.constant 0 255)
  guard (n1 /= n2)
  return
    ( Number "differentConstants" (fromIntegral n1),
      Number "differentConstants" (fromIntegral n2)
    )

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

deadExpression :: Gen (Expr BuildOut)
deadExpression = Number "dead" . fromIntegral <$> Hog.int (Hog.Range.constant 1 255)

bimapF :: Applicative f => (a -> f a) -> (a, a) -> f (a, a)
bimapF f (x, y) = (,) <$> f x <*> f y

ifTrue :: Expr BuildOut -> Gen (Expr BuildOut)
ifTrue e = do
  condition <- Number "condT" . fromIntegral <$> Hog.int (Hog.Range.constant 1 255)
  falseBranch <- deadExpression
  return (Cond "ifT" condition e falseBranch)

ifFalse :: Expr BuildOut -> Gen (Expr BuildOut)
ifFalse e = do
  trueBranch <- deadExpression
  return (Cond "ifF" (Number "condF" 0) trueBranch e)

or0 :: Expr BuildOut -> Gen (Expr BuildOut)
or0 e = pure (BinOp "or0" e BinOr (Number "or0" 0))


singleExprModule :: Identifier -> Expr BuildOut -> ModDecl BuildOut
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
