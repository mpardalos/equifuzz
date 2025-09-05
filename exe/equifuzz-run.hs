{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import CLI (
  RunnerOptions,
  runOptParse,
  runnerConfigOpts,
  runnerOptionsToRunner,
 )
import Control.Monad (when)
import Data.List (find)
import Data.Map qualified as Map
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID qualified as UUID
import Experiments (
  Evaluation (..),
  Experiment (..),
  ExperimentId (..),
  ExperimentResult (..),
  RunSystemCProgramResult (..),
  SignatureF (..),
  mkComparisonValueWithWidth,
  runSystemCProgram,
  showEvaluation,
 )
import Optics ((&))
import Options.Applicative qualified as Opt
import Safe (fromJustNote, readMay)
import SystemC qualified as SC
import Util (whenJust)

data Args
  = Args
  { path :: FilePath
  , runnerOpts :: RunnerOptions
  }

argParser :: Opt.Parser Args
argParser =
  Args
    <$> scFilePath
    <*> runnerConfigOpts
 where
  scFilePath = Opt.argument Opt.str (Opt.metavar "FILE")

main :: IO ()
main = do
  Args{path, runnerOpts} <-
    runOptParse
      "Attempt to construct and run an experiment from a SystemC file"
      argParser
  runner <- runnerOptionsToRunner runnerOpts
  experiment <- tryReadExperimentFromCPP =<< T.readFile path
  result <- runner experiment
  reportExperiment experiment result

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
