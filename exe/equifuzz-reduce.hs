{-# LANGUAGE LambdaCase #-}

module Main where

import CLI (RunnerOptions, runOptParse, runnerConfigOpts, runnerOptionsToRunner)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan)
import Data.Aeson (decodeFileStrict)
import Data.Text.IO qualified as TIO
import Experiments (Experiment, ExperimentResult (..), reportExperiment)
import Options.Applicative qualified as Opt
import Orchestration
import Safe (fromJustNote)
import WebUI (ExperimentProgress (..))

data Args
  = Args
  { path :: FilePath
  , runnerOpts :: RunnerOptions
  }

argParser :: Opt.Parser Args
argParser =
  Args
    <$> experimentDir
    <*> runnerConfigOpts
 where
  experimentDir =
    Opt.argument
      Opt.str
      ( Opt.metavar "FILE"
          <> Opt.help "Path to experiment JSON"
      )

main :: IO ()
main = do
  Args{path, runnerOpts} <-
    runOptParse
      "Attempt to construct and run an experiment from a SystemC file"
      argParser
  runner <- runnerOptionsToRunner runnerOpts
  experiment <-
    fromJustNote "Failed to parse experiment"
      <$> decodeFileStrict path
  chan <- newTChanIO
  startRunReduceThread chan runner experiment
  handleProgress experiment Nothing chan
 where
  handleProgress :: Experiment -> Maybe ExperimentResult -> TChan ExperimentProgress -> IO ()
  handleProgress experiment mResult chan =
    atomically (readTChan chan) >>= \case
      ExperimentStarted _ newExperiment ->
        handleProgress newExperiment mResult chan
      ExperimentCompleted _ result ->
        handleProgress experiment (Just result) chan
      ExperimentSequenceCompleted _ -> do
        let result = fromJustNote "Missing result" mResult
        reportExperiment experiment result
        putStrLn "----------------------"
        TIO.putStrLn result.fullOutput
