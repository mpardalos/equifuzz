{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Alternative ((<|>)), optional, (<**>))
import Data.Text.IO qualified as T
import Experiments
import GenSystemC (GenConfig (..))
import Options.Applicative qualified as Opt
import Orchestration
import WebUI (runWebUI)
#ifdef EVALUATION_VERSION
import Control.Monad.Random (setStdGen, mkStdGen)
#endif
import Data.Version
import Paths_equifuzz (version)

main :: IO ()
main = do
#ifdef EVALUATION_VERSION
  -- We set a fixed seed for the evaluation version
  setStdGen (mkStdGen 120)
#endif
  parseArgs >>= \case
    Web orchestrationConfig -> do
      progressChan <- startRunners orchestrationConfig
      runWebUI progressChan
    Generate genConfig -> do
      Experiment {design, designDescription, comparisonValue} <- mkSystemCConstantExperiment genConfig
      T.putStrLn design.source
      putStrLn "---------"
      T.putStrLn designDescription
      putStrLn "---------"
      T.putStrLn comparisonValue
    PrintVersion -> do
#ifdef EVALUATION_VERSION
      putStrLn (showVersion version <> " (evaluation)")
#else
      putStrLn (showVersion version <> " (full)")
#endif

--------------------------- CLI Parser -----------------------------------------

data Command
  = Web OrchestrationConfig
  | Generate GenConfig
  | PrintVersion

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "web" $
        Opt.info
          (Web <$> orchestrationConfigOpts)
          (Opt.progDesc "Run the equifuzz Web UI, connected to a remote host"),
      Opt.command "generate" $
        Opt.info
          (Generate <$> generateConfigOpts)
          (Opt.progDesc "Generate a sample of a generator"),
      Opt.command "version" $
        Opt.info
          (pure PrintVersion)
          (Opt.progDesc "Generate a sample of a generator")
    ]
  where
    orchestrationConfigOpts = do
      maxExperiments <-
        Opt.option Opt.auto . mconcat $
          [ Opt.long "max-experiments",
            Opt.metavar "COUNT",
            Opt.help "Maximum number of experiments to allow to run concurrently",
            Opt.value 10,
            Opt.showDefault
          ]
      verbose <-
        Opt.switch . mconcat $
          [ Opt.long "verbose",
            Opt.help "Print experiment status to the console"
          ]
      runnerConfig <- runnerConfigOpts <|> testFlag
      genConfig <- generateConfigOpts
      return
        OrchestrationConfig
          { runnerConfig,
            verbose,
            generatorThreads = 10,
            maxExperiments,
            -- Double the max concurrent experiments to make sure that we are never
            experimentQueueDepth = 20,
            genConfig
          }

    testFlag =
      Opt.flag' TestRunner . mconcat $
        [ Opt.long "test",
          Opt.help "Show test data on the interface, don't run any fuzzing"
        ]

    generateConfigOpts :: Opt.Parser GenConfig
    generateConfigOpts =
      GenConfig
        <$> ( Opt.option Opt.auto . mconcat $
                [ Opt.long "gen-steps",
                  Opt.metavar "COUNT",
                  Opt.help "Number of generation steps to use for each experiment",
                  Opt.value 30,
                  Opt.showDefault
                ]
            )

    runnerConfigOpts = do
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
      password <-
        optional . Opt.strOption . mconcat $
          [ Opt.long "password",
            Opt.metavar "PASSWORD",
            Opt.help "Password to connect to remote host"
          ]
      activatePath <-
        optional . Opt.strOption . mconcat $
          [ Opt.long "activate-script",
            Opt.metavar "PATH",
            Opt.help "Script to be sourced on the remote host before running vcf"
          ]

      return
        ( RunnerConfig . ExperimentRunner $
            runVCFormal
              SSHConnectionTarget {username, host, password}
              activatePath
        )

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
