{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use unless" #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import CLI
import GenSystemC (GenConfig (..))
import Meta (versionName)
import Options.Applicative qualified as Opt
import Orchestration
import Runners
import WebUI (runWebUI)

data WebOptions = WebOptions
  { verbose :: Bool
  , saveResults :: Bool
  , maxConcurrentExperiments :: Int
  , experimentCount :: Maybe Int
  , runnerOptions :: RunnerOptions
  , genSteps :: Int
  , evaluations :: Int
  , showVersion :: Bool
  }

data FECType = VCF | Jasper | SLEC

commandParser :: Opt.Parser WebOptions
commandParser = do
  maxConcurrentExperiments <-
    Opt.option Opt.auto . mconcat $
      [ Opt.long "max-concurrent"
      , Opt.metavar "COUNT"
      , Opt.help "Maximum number of experiments to allow to run concurrently"
      , Opt.value 10
      , Opt.showDefault
      ]
  experimentCount <-
    Opt.optional . Opt.option Opt.auto . mconcat $
      [ Opt.long "experiment-count"
      , Opt.metavar "COUNT"
      , Opt.help "Only run this many experiments"
      , Opt.showDefault
      ]
  verbose <-
    Opt.switch . mconcat $
      [ Opt.long "verbose"
      , Opt.help "Print experiment status to the console"
      ]
  showVersion <-
    Opt.switch . mconcat $
      [ Opt.long "version"
      , Opt.help "Print experiment status to the console"
      ]
  saveResults <-
    not
      <$> ( Opt.switch . mconcat $
              [ Opt.long "no-save"
              , Opt.help "Do not save successful experiment results"
              ]
          )
  evaluations <- evaluationsFlag
  genSteps <- genStepsFlag
  runnerOptions <- runnerConfigOpts
  return
    WebOptions
      { verbose
      , saveResults
      , maxConcurrentExperiments
      , experimentCount
      , runnerOptions
      , genSteps
      , evaluations
      , showVersion
      }
 where
  evaluationsFlag :: Opt.Parser Int
  evaluationsFlag =
    Opt.option Opt.auto . mconcat $
      [ Opt.long "evaluations"
      , Opt.metavar "COUNT"
      , Opt.help "How many inputs to evaluate the generated design at"
      , Opt.value 10
      , Opt.showDefault
      ]

  genStepsFlag :: Opt.Parser Int
  genStepsFlag =
    Opt.option Opt.auto . mconcat $
      [ Opt.long "gen-steps"
      , Opt.metavar "COUNT"
      , Opt.help "Number of generation steps to use for each experiment"
      , Opt.value 30
      , Opt.showDefault
      ]

main :: IO ()
main = do
  webOpts <- runOptParse "Run the equifuzz Web UI" commandParser
  if webOpts.showVersion
    then putStrLn versionName
    else do
      config <- webOptionsToOrchestrationConfig webOpts
      progressChan <- startRunners config
      runWebUI progressChan

webOptionsToOrchestrationConfig :: WebOptions -> IO OrchestrationConfig
webOptionsToOrchestrationConfig
  WebOptions
    { verbose
    , saveResults
    , maxConcurrentExperiments
    , experimentCount
    , genSteps
    , runnerOptions
    , evaluations
    } = do
    runner <- runnerOptionsToRunner runnerOptions
    let genConfig =
          GenConfig
            { growSteps = genSteps
            , genMods = runnerOptions.ecConfig.mods
            , evaluations
            }
    return
      OrchestrationConfig
        { verbose
        , saveResults
        , maxConcurrentExperiments
        , experimentCount
        , genConfig
        , runner
        }
