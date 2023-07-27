{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Main where

import Control.Applicative (Alternative (empty, (<|>)), optional, (<**>))
import Data.Text.IO qualified as T
import Experiments
import GenSystemC (GenConfig (..))
import Options.Applicative qualified as Opt
import Orchestration
import WebUI (runWebUI)

orchestrationConfig :: RunnerConfig -> OrchestrationConfig
orchestrationConfig runnerConfig =
  OrchestrationConfig
    { runnerConfig,
      logging = True,
      generatorThreads = 10,
      maxExperiments = 1,
      -- Double the max concurrent experiments to make sure that we are never
      experimentQueueDepth = 20,
      genConfig
    }

genConfig :: GenConfig
genConfig =
  GenConfig
    { growSteps = 30
    }

main :: IO ()
main =
  parseArgs >>= \case
    Web runnerConfig -> do
      progressChan <- startRunners (orchestrationConfig runnerConfig)
      runWebUI progressChan
    Generate -> do
      Experiment {design, designDescription, comparisonValue} <- mkSystemCConstantExperiment genConfig
      T.putStrLn design.source
      putStrLn "---------"
      T.putStrLn designDescription
      putStrLn "---------"
      T.putStrLn comparisonValue

--------------------------- CLI Parser -----------------------------------------

data Command
  = Web RunnerConfig
  | Generate

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "web" $
        Opt.info
          (Web <$> (testFlag <|> runnerConfig))
          (Opt.progDesc "Run the equifuzz Web UI, connected to a remote host"),
      Opt.command "generate" $
        Opt.info
          (pure Generate)
          (Opt.progDesc "Generate a sample of a generator")
    ]
  where
    testFlag =
      Opt.flag' TestRunner . mconcat $
        [ Opt.long "test",
          Opt.help "Show test data on the interface, don't run any fuzzing"
        ]

    runnerConfig = do
      host <-
        Opt.strOption . mconcat $
          [ Opt.long "host",
            Opt.metavar "HOSTNAME",
            Opt.help "Remote hostname or IP to run equivalence checker on"
          ]
      username <-
        Opt.strOption . mconcat $
          [ Opt.long "username",
            Opt.metavar "USERNAME",
            Opt.help "Username to connect to remote host"
          ]
      activatePath <-
        optional . Opt.strOption . mconcat $
          [ Opt.long "activate-script",
            Opt.metavar "PATH",
            Opt.help "Script to be sourced on the remote host before running vcf"
          ]

      return $ RunnerConfig $ ExperimentRunner (runVCFormal username host activatePath)

parseArgs :: IO Command
parseArgs =
  Opt.execParser $
    Opt.info
      (commandParser <**> Opt.helper)
      ( mconcat
          [ Opt.fullDesc,
            Opt.progDesc "Fuzzer for formal equivalence checkers"
          ]
      )
