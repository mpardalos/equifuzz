{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildOut.SystemC where

import BuildOut.Internal
import Control.Applicative (Alternative)
import Control.Monad (guard)
import Control.Monad.Accum
import Control.Monad.State (MonadState (state), StateT (runStateT))
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hedgehog (Gen)
import Hedgehog qualified as Hog
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (both, over, view, (%), (%~), (&), (.~))
import Verismith.Verilog.AST

maxWireSize :: Int
maxWireSize = 512

newInput :: Int -> BuildOutM Text
newInput size = do
  existingPortNames <- map (view (#name % #getIdentifier)) <$> look
  portName <-
    Hog.filterT (not . (`elem` existingPortNames)) $
      Hog.text (Hog.Range.singleton 5) Hog.lower
  let port = Port Wire False size (Identifier portName)
  add [port]
  return port

-- | Increase the size and complexity of an expression while preserving its semantics
grow :: ExprPair -> BuildOutM ExprPair
grow pair = do
  count <- Hog.int (Hog.Range.linear 1 20)
  iterateM count grow1 pair
  where
    grow1 (e1, e2) = do
      -- FIXME: Make the random parts be identical for both expressions
      f <-
        Hog.element
          [ ifFalse,
            ifTrue,
            pure . signedUnsigned,
            pure . unsignedSigned,
            or0
            -- TODO: Implement bit-select entire vector
          ]
      e1' <- f e1
      e2' <- f e2
      return (e1', e2')

deadExpression :: BuildOutM (Expr BuildOut)
deadExpression = Number "dead" . fromIntegral <$> Hog.int (Hog.Range.constant (-255) 255)

mapBothA :: Applicative f => (a -> f b) -> (a, a) -> f (b, b)
mapBothA f = bimapA f f

bimapA :: Applicative f => (a -> f a') -> (b -> f b') -> (a, b) -> f (a', b')
bimapA f g (x, y) = (,) <$> f x <*> g y

ifTrue :: Expr BuildOut -> BuildOutM (Expr BuildOut)
ifTrue e = do
  condition <- Number "condT" . fromIntegral <$> Hog.int (Hog.Range.constant 1 255)
  falseBranch <- deadExpression
  return (Cond "ifT" condition e falseBranch)

ifFalse :: Expr BuildOut -> BuildOutM (Expr BuildOut)
ifFalse e = do
  trueBranch <- deadExpression
  return (Cond "ifF" (Number "condF" 0) trueBranch e)

or0 :: Expr BuildOut -> BuildOutM (Expr BuildOut)
or0 e = pure (BinOp "or0" e BinOr (Number "or0" 0))

signedUnsigned :: Expr BuildOut -> Expr BuildOut
signedUnsigned e = Appl "signedUnsigned" "$signed" (Appl "signedUnsigned" "$unsigned" e)

unsignedSigned :: Expr BuildOut -> Expr BuildOut
unsignedSigned e = Appl "unsignedSigned" "$unsigned" (Appl "unsignedSigned" "$signed" e)

singleExprModule :: Identifier -> [Port BuildOut] -> Expr BuildOut -> ModDecl BuildOut
singleExprModule name inPorts e =
  ModDecl
    { annotation = (),
      params = [],
      id = name,
      inPorts,
      outPorts = [outPort],
      items = portDecls ++ [ModCA () (ContAssign "y" e)]
    }
  where
    portDecls =
      Decl () (Just PortOut) outPort Nothing
        : [ Decl () (Just PortIn) port Nothing
            | port <- inPorts
          ]
    outPort =
      Port
        { portType = Wire,
          signed = False,
          size = Range (ConstNum () (fromIntegral $ maxWireSize - 1)) (ConstNum () 0),
          name = "y"
        }

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ x = pure x
iterateM 1 f x = f x
iterateM n f x = do
  x' <- f x
  iterateM (n - 1) f x'
