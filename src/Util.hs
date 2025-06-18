module Util where

import Control.Concurrent (MVar, forkFinally, modifyMVar_)
import Control.Monad (void, when)
import Control.Monad.Random (foldM_, forever)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (NominalDiffTime, defaultTimeLocale, getZonedTime, zonedTimeToLocalTime)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Conc (labelThread)
import Optics (Prism', preview)
import Shelly (Sh)
import Shelly qualified as Sh
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ x = pure x
iterateM 1 f x = f x
iterateM n f x = do
  x' <- f x
  iterateM (n - 1) f x'

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

reportError :: String -> String -> IO ()
reportError title err = do
  time <- zonedTimeToLocalTime <$> getZonedTime
  hPutStrLn stderr (printf "[%s] %s" (iso8601Show time) title)
  hPutStrLn stderr "========================="
  hPutStrLn stderr err
  hPutStrLn stderr "========================="
  hPutStrLn stderr ""

foreverThread :: String -> IO a -> IO ()
foreverThread title action = do
  tid <-
    forkFinally
      (forever action)
      ( \result -> do
          case result of
            Right () ->
              reportError
                (printf "Thread ended: '%s'" title)
                "Ended successfully, but unexpectedly"
            Left e ->
              reportError
                (printf "Error in thread: '%s'" title)
                (show e)
          foreverThread title action
      )
  labelThread tid title

-- | Like `foldM_`, but if the operation returns True, break (i.e. do not process
-- the remaining elements).
foldMUntil_ :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m ()
foldMUntil_ action =
  foldM_
    ( \done acc ->
        if done
          then pure done
          else action acc
    )
    False

mwhen :: Monoid a => Bool -> a -> a
mwhen True a = a
mwhen False _ = mempty

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond action = do
  v <- cond
  when v action

modifyMVarPure_ :: MVar a -> (a -> a) -> IO ()
modifyMVarPure_ var f = modifyMVar_ var (pure . f)

diffTimeHMSFormat :: NominalDiffTime -> String
diffTimeHMSFormat = formatTime defaultTimeLocale "%h:%M:%S"

bashExec :: Text -> Sh Text
bashExec commands = Sh.run "bash" ["-c", commands]

bashExec_ :: Text -> Sh ()
bashExec_ = void . bashExec

is :: a -> Prism' a b -> Bool
is x p = isJust $ preview p x

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) f a = fmap f <$> a
