module Main where

import Brick.BChan qualified as B
import Control.Concurrent (forkFinally)
import Control.Monad (void)
import Experiments
import TUI (AppEvent (..), runTUI)

experimentThread :: B.BChan AppEvent -> IO ()
experimentThread eventChan =
  void $
    forkFinally
      (experimentLoop mkNegativeExperiment runVCFormal (B.writeBChan eventChan . ExperimentProgress))
      ( const $ do
          B.writeBChan eventChan ExperimentThreadCrashed
          experimentThread eventChan
      )

main :: IO ()
main = do
  eventChan <- B.newBChan 10
  experimentThread eventChan
  runTUI eventChan
