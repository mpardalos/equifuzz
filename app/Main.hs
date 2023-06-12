{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Main where

import Control.Applicative ((<**>))
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
    { growSteps = 300
    }

webMain :: Bool -> IO ()
webMain test = do
  progressChan <- startRunners (orchestrationConfig {test})
  runWebUI progressChan

genMain :: IO ()
genMain = do
  Experiment {design, comparisonValue} <- mkSystemCConstantExperiment genConfig
  T.putStrLn design.source
  putStrLn "---------"
  T.putStrLn comparisonValue

main :: IO ()
main =
  parseArgs >>= \case
    Web {test} -> webMain test
    Generate -> genMain

--------------------------- CLI Parser -----------------------------------------

data Command
  = Web {test :: Bool}
  | Generate

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "web" $
        Opt.info
          (Web <$> testFlag)
          (Opt.progDesc "Run the equifuzz TUI, connected to a remote host"),
      Opt.command "generate" $
        Opt.info
          (pure Generate)
          (Opt.progDesc "Generate a sample of a generator")
    ]
  where
    testFlag =
      Opt.switch
        ( Opt.long "test"
            <> Opt.help "Show test data on the interface, don't run any fuzzing"
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
