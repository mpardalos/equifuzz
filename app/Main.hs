module Main where

import Brick.BChan qualified as B
import Control.Concurrent (forkFinally)
import Control.Monad (replicateM_, void)
import Data.Text.IO qualified as T
import Experiments
import System.Environment (getArgs, getProgName)
import TUI (AppEvent (..), runTUI)
import Text.Printf (printf)
import Verismith.Verilog (genSource)

tuiMain :: IO ()
tuiMain = do
  eventChan <- B.newBChan 20
  replicateM_ 10 $ experimentThread eventChan
  runTUI eventChan
  where
    experimentThread :: B.BChan AppEvent -> IO ()
    experimentThread eventChan =
      void $
        forkFinally
          (experimentLoop mkBuildOutExperiment runVCFormal (B.writeBChan eventChan . ExperimentProgress))
          (const $ experimentThread eventChan)

genMain :: IO ()
genMain = do
  Experiment {design1, design2} <- mkBuildOutExperiment
  T.putStrLn (genSource design1)
  putStrLn "---------"
  T.putStrLn (genSource design2)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["tui"] -> tuiMain
    [] -> tuiMain
    ["generate"] -> genMain
    _ -> do
      name <- getProgName
      printf "Usage: %s [command]\n" name
      printf "Commands:\n"
      printf "  tui       : (default) Run the TUI\n"
      printf "  generate  : Generate an experiment and write it to stdout\n"
