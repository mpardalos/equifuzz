module Main where

import Brick.BChan qualified as B
import Control.Concurrent (forkFinally)
import Control.Monad (replicateM_, void)
import Data.Text.IO qualified as T
import Experiments
import System.Environment (getArgs, getProgName)
import TUI (AppEvent (..), runTUI)
import Text.Printf (printf)

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
          (experimentLoop mkSystemCVerilogExperiment runVCFormal (B.writeBChan eventChan . ExperimentProgress))
          (const $ experimentThread eventChan)

genMain :: String -> IO ()
genMain "systemc-verilog" = do
  Experiment {design1, design2} <- mkSystemCVerilogExperiment
  T.putStrLn design1.source
  putStrLn "---------"
  T.putStrLn design2.source
genMain "systemc-constant" = do
  Experiment {design1} <- mkSystemCConstantExperiment
  T.putStrLn design1.source
genMain t = printf "Unknown generate type: '%s'" t

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["tui"] -> tuiMain
    [] -> tuiMain
    ["generate", genType] -> genMain genType
    _ -> do
      name <- getProgName
      printf "Usage: %s [command]\n" name
      printf "Commands:\n"
      printf "  tui                (default) Run the TUI\n"
      printf "  generate <type>    Generate an experiment and write it to stdout\n"
      printf "Generate types:\n"
      printf "  - systemc-verilog\n"
      printf "  - systemc-constant\n"
