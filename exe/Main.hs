{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use unless" #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM_, replicateM_, when)
import Control.Monad.Random (getStdRandom)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID qualified as UUID
import Experiments
import GenSystemC (GenConfig (..), GenMods)
import Meta (versionName)
import Optics (isn't, only, (%), (&), _Right)
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
import Data.List (find)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Safe (fromJustNote, readMay)
import Util (whenJust)

main :: IO ()
main = do
  parseArgs >>= \case
    Diy filepath runnerOptions -> do
      runner <- runnerOptionsToRunner runnerOptions
      experiment <- tryReadExperimentFromCPP =<< T.readFile filepath
      result <- runner experiment
      reportExperiment experiment result
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
        mapM_
          (T.putStrLn . showEvaluation scSignature)
          knownEvaluations
    PrintVersion -> putStrLn versionName

reportExperiment :: Experiment -> ExperimentResult -> IO ()
reportExperiment experiment result = do
  T.putStrLn "--- spec.cpp --------------------------------"
  T.putStrLn experiment.scDesign
  T.putStrLn "---"
  mapM_
    (T.putStrLn . showEvaluation experiment.scSignature)
    experiment.knownEvaluations

  T.putStrLn "--- impl.sv --------------------------------"
  T.putStrLn experiment.verilogDesign

  -- T.putStrLn "--- Output --------------------------------"
  -- T.putStrLn result.fullOutput
  T.writeFile "log.txt" result.fullOutput

  whenJust result.counterExample $ \cex -> do
    T.putStrLn "--- Counter-Example -----------------------"
    T.putStrLn cex

  T.putStrLn "-------------------------------------------"
  T.putStr "Result: "
  T.putStrLn $
    case result.proofFound of
      Just True -> "Equivalent"
      Just False -> "Non-equivalent"
      Nothing -> "Inconclusive"

-- | Commit some parsing crimes, try to read a design from a CPP file,
-- simulate it, and construct an experiment comparing it against the expected result
-- The returned Experiment is not fully initialized, but it should be good enough.
tryReadExperimentFromCPP :: Text -> IO Experiment
tryReadExperimentFromCPP source = do
  let returnType =
        source
          & T.lines
          & find (\l -> "dut(" `T.isInfixOf` l)
          & fromJustNote "No dut function found"
          & T.breakOn "dut("
          & fst
          & T.strip

  when (T.null returnType) $
    error "Could not extract type of dut function"

  let scSignature = SC.Signature{name = "dut", args = [], returnType}
  let runDut :: Text
        | "sc" `T.isInfixOf` returnType =
            [__i|
              std::cout << dut().length() << std::endl;
              std::cout << dut().to_string(sc_dt::SC_BIN, false) << std::endl;
            |]
        | returnType == "double" =
            [__i|
              double valDouble = dut();
              unsigned long long valUnsigned;
              std::memcpy(&valUnsigned, &valDouble, sizeof(valDouble));
              std::cout << 64 << std::endl;
              std::cout << std::bitset<64>(valUnsigned) << std::endl;
            |]
        | returnType == "bool" =
            [__i|
              std::cout << 1 << std::endl;
              std::cout << (dut() ? 1 : 0) << std::endl;
            |]
        | otherwise =
            [__i|
              std::cout << sizeof(decltype(dut())) * 8 << std::endl;
              std::cout << std::bitset<sizeof(decltype(dut())) * 8>(dut()) << std::endl;
            |]

  let simulationSource =
        [__i|
            \#include <systemc>
            \#include<iostream>
            \#include<bitset>

            #{source}

            int sc_main(int, char**) {
              #{runDut}
            }
            |]
  RunSystemCProgramResult{..} <- runSystemCProgram simulationSource
  let (width :: Int, value) =
        case T.lines programStdOut of
          [readMay . T.unpack -> Just w, v] -> (w, v)
          _ -> error "Unexpected output from simulation"

  let verilogDesign :: Text =
        [__i|
            module top(output reg [#{width - 1}:0] out);
              assign out = #{width}'b#{value};
            endmodule
            |]

  return
    Experiment
      { experimentId = ExperimentId UUID.nil
      , expectedResult = True
      , scSignature
      , scDesign = source
      , verilogDesign
      , size = 0
      , generateProcess = error "Parsed design has no generateProcess"
      , knownEvaluations =
          [ Evaluation
              { inputs = []
              , output = mkComparisonValueWithWidth width value
              }
          ]
      , extraInfos = Map.empty
      }

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

showEvaluation :: TextSignature -> Evaluation -> Text
showEvaluation sig Evaluation{inputs, output} =
  case inputs of
    [] -> "* -> " <> comparisonValueRaw output
    (_ : _) ->
      "* "
        <> T.intercalate
          "\n  "
          ( [ name <> "=" <> comparisonValueRaw value
            | ((_, name), value) <- zip sig.args inputs
            ]
          )
        <> " -> \n  "
        <> comparisonValueRaw output

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
      , extraInfos = Map.empty
      }
