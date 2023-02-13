module Main where

import Brick.BChan qualified as B
import Control.Concurrent (forkFinally)
import Control.Monad (void)
import Experiments
import TUI (runTUI)

experimentThread :: B.BChan ExperimentProgress -> IO ()
experimentThread eventChan =
  void $
    forkFinally
      (experimentLoop mkNegativeExperiment runVCFormal (B.writeBChan eventChan))
      (const $ experimentThread eventChan)

main :: IO ()
main = do
  eventChan <- B.newBChan 10
  experimentThread eventChan
  runTUI eventChan
