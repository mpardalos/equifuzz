module Util where

import Control.Concurrent (forkFinally)
import Control.Monad (void)
import Data.Time (getZonedTime, zonedTimeToLocalTime)
import Data.Time.Format.ISO8601 (iso8601Show)
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

errorLog :: FilePath
errorLog = "equifuzz.error.log"

reportError :: String -> String -> IO ()
reportError title err = do
  time <- zonedTimeToLocalTime <$> getZonedTime
  appendFile errorLog . unlines $
    [ printf "[%s] %s" (iso8601Show time) title,
      "=========================",
      err,
      "=========================",
      ""
    ]

forkRestarting :: String -> IO () -> IO ()
forkRestarting title action =
  void $
    forkFinally
      action
      ( \err -> do
          reportError title (show err)
          forkRestarting title action
      )
