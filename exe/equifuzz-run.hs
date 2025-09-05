module Main where

import CLI (RunnerOptions, runOptParse, runnerConfigOpts, runnerOptionsToRunner)
import Data.Aeson (decodeFileStrict)
import Data.Text.IO qualified as TIO
import Experiments (ExperimentResult (..), reportExperiment)
import Options.Applicative qualified as Opt
import Safe (fromJustNote)

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
  result <- runner experiment
  reportExperiment experiment result
  putStrLn "----------------------"
  TIO.putStrLn result.fullOutput
