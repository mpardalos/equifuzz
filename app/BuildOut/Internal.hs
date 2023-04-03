{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildOut.Internal where

import Control.Applicative (Alternative)
import Control.Monad.State (MonadState, StateT (runStateT))
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog qualified as Hog
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (makeFieldLabelsNoPrefix, use)
import Optics.State.Operators ((%=))
import SystemC qualified as SC
import Verismith.Verilog.AST qualified as V

data InputPort = InputPort
  { width :: Int,
    name :: Text
  }

makeFieldLabelsNoPrefix ''InputPort

data BuildOutState s = BuildOutState
  { inputPorts :: [InputPort],
    nextInputPortIdx :: Int,
    extraState :: s
  }

makeFieldLabelsNoPrefix ''BuildOutState

-- | Monad for building out test programs inside-out. It provides `MonadGen` and
-- `MonadAccum`. The idea
--
-- What we really want here is AccumT, but there are no instances for `AccumT w
-- Gen`, so we instead we use StateT (which is isomorphic to AccumT) and provide
-- MonadAccum instance
newtype BuildOutM s a = BuildOutM (StateT (BuildOutState s) Gen a)
  deriving newtype (Applicative, Monad, Alternative, Hog.MonadGen)
  deriving stock (Functor)

deriving newtype instance MonadState (BuildOutState s) (BuildOutM s)

initState :: s -> BuildOutState s
initState s =
  BuildOutState
    { inputPorts = [],
      nextInputPortIdx = 0,
      extraState = s
    }

runBuildOutM :: BuildOutM s a -> BuildOutState s -> Gen (a, BuildOutState s)
runBuildOutM (BuildOutM m) = runStateT m

newInputPort :: Int -> BuildOutM s InputPort
newInputPort size = do
  portIdx <- use #nextInputPortIdx
  #nextInputPortIdx %= (+ 1)
  let port = InputPort size ("in" <> T.pack (show portIdx))
  #inputPorts %= (++ [port])
  return port

inputPortAsVerilog :: V.Annotation a => InputPort -> V.Port a
inputPortAsVerilog InputPort {width, name} =
  V.Port
    V.Wire
    False
    (V.rangeFromSize (fromIntegral width))
    (V.Identifier name)

inputPortAsSystemC :: InputPort -> (SC.SCType, Text)
inputPortAsSystemC InputPort {width, name} = (SC.SCUInt width, name)

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ x = pure x
iterateM 1 f x = f x
iterateM n f x = do
  x' <- f x
  iterateM (n - 1) f x'

-- | Maximum size in bits for any given signal generated. This limitation is set
-- by Hector for the output signal, so it should be easy to remove
maxWireSize :: Int
maxWireSize = 64

wireSize :: Hog.MonadGen m => m Int
wireSize = Hog.int (Hog.Range.linear 1 maxWireSize)
