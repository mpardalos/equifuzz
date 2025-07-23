{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# HLINT ignore "Use unless" #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM_, replicateM_, when)
import Control.Monad.Random (getStdRandom)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Experiments
import GenSystemC (GenConfig (..), GenMods)
import Meta (versionName)
import Optics (isn't, only, (%), _Right)
import Orchestration
import Runners
import System.Random (uniformR)
import SystemC qualified as SC
import ToolRestrictions (jasperMods, noMods, slecMods, vcfMods)
import WebUI (runWebUI)

import CLI (
  Command (..),
  FECType (..),
  GenerateOptions (..),
  PasswordSource (..),
  RunnerOptions (..),
  SSHOptions (..),
  WebOptions (..),
  askPassword,
  parseArgs,
 )

main :: IO ()
main = do
  parseArgs >>= \case
    Web webOpts -> do
      config <- webOptionsToOrchestrationConfig webOpts
      progressChan <- startRunners config
      runWebUI progressChan
    Generate genOpts -> do
      replicateM_ genOpts.count $ do
        Experiment{scSignature, scDesign, verilogDesign, knownEvaluations} <-
          genSystemCConstantExperiment (generateOptionsToGenConfig genOpts)
        T.putStrLn scDesign
        putStrLn "---------"
        T.putStrLn verilogDesign
        putStrLn "---------"
        forM_ knownEvaluations $ \Evaluation{inputs, output} -> do
          T.putStr "\n*\t"
          T.putStr $
            T.intercalate "\n\t" $
              [ name <> "=" <> comparisonValueRaw value
              | ((_, name), value) <- zip scSignature.args inputs
              ]
          T.putStrLn ""
          T.putStr "\t-> "
          T.putStr (comparisonValueRaw output)
    PrintVersion -> putStrLn versionName

generateOptionsToGenConfig :: GenerateOptions -> GenConfig
generateOptionsToGenConfig GenerateOptions{genSteps, evaluations} =
  GenConfig
    { growSteps = genSteps
    , transformationAllowed = noMods
    , evaluations
    }

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
            , transformationAllowed = runnerOptionsToGenMods runnerOptions
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

runnerOptionsToGenMods :: RunnerOptions -> GenMods
runnerOptionsToGenMods Runner{fecType = VCF} = vcfMods
runnerOptionsToGenMods Runner{fecType = Jasper} = jasperMods
runnerOptionsToGenMods Runner{fecType = SLEC} = slecMods
runnerOptionsToGenMods TestRunner{} = noMods

runnerOptionsToRunner :: RunnerOptions -> IO ExperimentRunner
runnerOptionsToRunner TestRunner{includeInconclusive} =
  return (testRunner includeInconclusive)
runnerOptionsToRunner
  Runner
    { sshOptions
    , fecType
    } = do
    -- host,
    -- username,
    -- passwordSource,
    -- activatePath,
    let ec = case fecType of
          VCF -> vcFormal
          Jasper -> jasper
          SLEC -> slec
    case sshOptions of
      Nothing -> return (runECLocal ec)
      Just SSHOptions{..} -> do
        password <- case passwordSource of
          NoPassword -> pure Nothing
          PasswordGiven pass -> pure (Just pass)
          AskPassword -> Just <$> askPassword
        let sshOpts = SSHConnectionTarget{username, host, password}
        putStrLn "Validating SSH connection..."
        sshValid <- try @SomeException $ validateSSH sshOpts
        when (isn't (_Right % only True) sshValid) $
          throwIO (userError "Could not connect to ssh host. Please check the options you provided")
        putStrLn "SSH connection OK"
        return $ runECRemote SSHConnectionTarget{..} activatePath ec

--------------------------- Testing --------------------------------------------

testRunner :: Bool -> ExperimentRunner
testRunner inconclusiveResults experiment = do
  getStdRandom (uniformR (1_000_000, 5_000_000)) >>= threadDelay

  proofFound <-
    getStdRandom (uniformR (1 :: Int, 100)) <&> \x ->
      if
        | x < 10 && inconclusiveResults -> Nothing
        | x < 20 -> Just (not experiment.expectedResult)
        | otherwise -> Just experiment.expectedResult

  let counterExample =
        if proofFound == Just False
          then Nothing
          else Just "Counter-example goes here"
  let fullOutput = "blah\nblah\nblah"

  return
    ExperimentResult
      { experimentId = experiment.experimentId
      , proofFound
      , counterExample
      , fullOutput
      }
