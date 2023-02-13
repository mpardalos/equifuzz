module Main where

import Brick.BChan qualified as B
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Experiments
import TUI (runTUI)

main :: IO ()
main = do
  eventChan <- B.newBChan 10

  -- Start experiment running thread
  void . forkIO $
    experimentLoop mkNegativeExperiment runVCFormal (B.writeBChan eventChan)

  -- UI/main thread
  runTUI eventChan
