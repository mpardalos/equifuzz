module Util where

import Control.Concurrent (forkFinally)
import Control.Monad.Random (forever)
import Data.Time (getZonedTime, zonedTimeToLocalTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Conc (labelThread)
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
