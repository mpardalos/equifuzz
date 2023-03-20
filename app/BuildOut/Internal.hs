{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildOut.Internal where

import Control.Applicative (Alternative)
import Control.Monad.Accum
import Control.Monad.State (MonadState (state), StateT (runStateT))
import Data.Text (Text)
import Hedgehog (Gen)
import Hedgehog qualified as Hog

data InputPort = InputPort
  { width :: Int,
    name :: Text
  }

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
