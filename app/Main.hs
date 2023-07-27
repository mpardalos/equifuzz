{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Main where

import Control.Applicative ((<**>))
import Data.Text (Text)
import Data.Text.IO qualified as T
import Experiments
import GenSystemC (GenConfig (..))
import Options.Applicative qualified as Opt
import Orchestration
import WebUI (runWebUI)

orchestrationConfig :: OrchestrationConfig
orchestrationConfig =
  OrchestrationConfig
    { runner = hector_2023_03_1,
      test = False,
      logging = True,
      generatorThreads = 10,
      maxExperiments = 10,
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
    Web {test} -> do
      progressChan <- startRunners (orchestrationConfig {test})
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
  = Web
      { test :: Bool,
        runnerHost :: Text,
        runnerUsername :: Text,
        runnerPass :: Text
      }
  | Generate

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "web" $
        Opt.info
          ( Web
              <$> testFlag
              <*> runnerHostOpt
              <*> runnerUsernameOpt
              <*> runnerPassOpt
          )
          (Opt.progDesc "Run the equifuzz TUI, connected to a remote host"),
      Opt.command "generate" $
        Opt.info
          (pure Generate)
          (Opt.progDesc "Generate a sample of a generator")
    ]
  where
    testFlag =
      Opt.switch . mconcat $
        [ Opt.long "test",
          Opt.help "Show test data on the interface, don't run any fuzzing"
        ]
    runnerHostOpt =
      Opt.strOption . mconcat $
        [ Opt.long "host",
          Opt.metavar "HOSTNAME",
          Opt.help "Remote hostname or IP to run equivalence checker on"
        ]
    runnerUsernameOpt =
      Opt.strOption . mconcat $
        [ Opt.long "username",
          Opt.metavar "USERNAME",
          Opt.help "Username to connect to remote host"
        ]
    runnerPassOpt =
      Opt.strOption . mconcat $
        [ Opt.long "password",
          Opt.metavar "PASSWORD",
          Opt.help "Password to connect to remote host (Default: no password)",
          Opt.value ""
        ]

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
