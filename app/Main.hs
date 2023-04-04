module Main where

import Brick.BChan qualified as B
import Control.Concurrent (forkFinally)
import Control.Monad (replicateM_, void)
import Data.Text.IO qualified as T
import Experiments
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import TUI (AppEvent (..), runTUI)
import Text.Printf (printf)

tuiMain :: IO Experiment -> IO ()
tuiMain generator = do
  eventChan <- B.newBChan 20
  replicateM_ 10 $ experimentThread eventChan
  runTUI eventChan
  where
    experimentThread :: B.BChan AppEvent -> IO ()
    experimentThread eventChan =
      void $
        forkFinally
          (experimentLoop generator runVCFormal (B.writeBChan eventChan . ExperimentProgress))
          (const $ experimentThread eventChan)

genMain :: IO Experiment -> IO ()
genMain generator = do
  Experiment {design1, design2} <- generator
  T.putStrLn design1.source
  putStrLn "---------"
  T.putStrLn design2.source

getExperimentGenerator :: String -> Maybe (IO Experiment)
getExperimentGenerator "systemc-constant" = Just mkSystemCConstantExperiment
getExperimentGenerator "systemc-verilog" = Just mkSystemCVerilogExperiment
getExperimentGenerator _ = Nothing

usageAndExit :: IO a
usageAndExit = do
  name <- getProgName
  printf "Usage: %s <command> <generate-type>\n" name
  printf "Commands:\n"
  printf "  tui                Run the TUI\n"
  printf "  generate           Generate an experiment and write it to stdout\n"
  printf "Generate types:\n"
  printf "  systemc-verilog\n"
  printf "  systemc-constant\n"
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  (command, genType) <- case args of
    [command, genType] -> pure (command, genType)
    _ -> usageAndExit
  generator <- case getExperimentGenerator genType of
    Just generator -> pure generator
    Nothing -> usageAndExit
  case command of
    "tui" -> tuiMain generator
    "generate" -> genMain generator
    _ -> usageAndExit
