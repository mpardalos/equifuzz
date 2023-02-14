module Main where

import Brick.BChan qualified as B
import Control.Concurrent (forkFinally)
import Control.Monad (void)
import Data.Text.IO qualified as T
import Experiments
import System.Environment (getArgs, getProgName)
import TUI (AppEvent (..), runTUI)
import Verismith.Verilog (genSource)
import Text.Printf (printf)

experimentThread :: B.BChan AppEvent -> IO ()
experimentThread eventChan =
  void $
    forkFinally
      (experimentLoop mkNegativeExperiment runVCFormal (B.writeBChan eventChan . ExperimentProgress))
      ( const $ do
          B.writeBChan eventChan ExperimentThreadCrashed
          experimentThread eventChan
      )

tuiMain :: IO ()
tuiMain = do
  eventChan <- B.newBChan 10
  experimentThread eventChan
  runTUI eventChan

genMain :: IO ()
genMain = do
  Experiment {design1, design2} <- mkNegativeExperiment
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
