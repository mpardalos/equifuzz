{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildOut.Internal where

import Control.Applicative (Alternative)
import Control.Monad.Accum (MonadAccum (..))
import Control.Monad.State (MonadState (state), StateT (runStateT))
import Data.Text (Text)
import Hedgehog (Gen)
import Hedgehog qualified as Hog
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (makeFieldLabelsNoPrefix, view)
import SystemC qualified as SC
import Verismith.Verilog.AST qualified as V

data InputPort = InputPort
  { width :: Int,
    name :: Text
  }

makeFieldLabelsNoPrefix ''InputPort

-- What we really want here is AccumT, but there are no instances for `AccumT w
-- Gen`, so we instead we use StateT (which is isomorphic to AccumT) and provide
-- MonadAccum instance
newtype BuildOutM a = BuildOutM (StateT [InputPort] Gen a)
  deriving newtype (Applicative, Monad, Alternative, Hog.MonadGen)
  deriving stock (Functor)

instance MonadAccum [InputPort] BuildOutM where
  accum f = BuildOutM . state $ \existing ->
    let (val, new) = f existing
     in (val, new <> existing)

runBuildOutM :: BuildOutM a -> Gen (a, [InputPort])
runBuildOutM (BuildOutM m) = runStateT m []

newInputPort :: Int -> BuildOutM InputPort
newInputPort size = do
  existingPortNames <- map (view #name) <$> look
  name <-
    Hog.filterT (not . (`elem` existingPortNames)) $
      Hog.text (Hog.Range.singleton 5) Hog.lower
  let port = InputPort size name
  add [port]
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

wireSize :: BuildOutM Int
wireSize = Hog.int (Hog.Range.linear 1 maxWireSize)

