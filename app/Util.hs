module Util where

import Control.Concurrent (forkFinally)
import Control.Monad (void)
import Data.Time (getZonedTime, zonedTimeToLocalTime)
import Data.Time.Format.ISO8601 (iso8601Show)
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

forkRestarting :: String -> IO () -> IO ()
forkRestarting title action =
  void $
    forkFinally
      action
      ( \result -> do
          case result of
            Right () -> reportError title "Thread ended"
            Left e -> reportError title (show e)
          forkRestarting title action
      )
