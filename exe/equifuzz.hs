{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use unless" #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative (Alternative ((<|>)), optional, (<**>))
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (when)
import Data.List (find)
import Data.Map qualified as Map
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID qualified as UUID
import Experiments
import GenSystemC (GenConfig (..))
import Meta (versionName)
import Optics (isn't, only, (%), (&), _Right)
import Options.Applicative qualified as Opt
import Orchestration
import Runners
import Safe (fromJustNote, readMay)
import System.IO (hFlush, hSetEcho, stdin, stdout)
import SystemC qualified as SC
import Text.Printf (printf)
import ToolRestrictions (noMods)
import Util (whenJust)
import WebUI (runWebUI)

data Command
  = Web WebOptions
  | Diy FilePath RunnerOptions
  | PrintVersion

data WebOptions = WebOptions
  { verbose :: Bool
  , saveResults :: Bool
  , maxConcurrentExperiments :: Int
  , experimentCount :: Maybe Int
  , runnerOptions :: RunnerOptions
  , genSteps :: Int
  , evaluations :: Int
  }

data PasswordSource = NoPassword | AskPassword | PasswordGiven Text

data FECType = VCF | Jasper | SLEC

data SSHOptions = SSHOptions
  { host :: Text
  , username :: Text
  , passwordSource :: PasswordSource
  , activatePath :: Maybe Text
  }

data RunnerOptions = RunnerOptions
  { sshOptions :: Maybe SSHOptions
  , ecConfig :: EquivalenceCheckerConfig
  }

askPassword :: IO Text
askPassword = do
  printf "Your password: "
  hFlush stdout
  hSetEcho stdin False
  password <- T.getLine
  hSetEcho stdin True
  putStr "\n"
  return password

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "web" $
        Opt.info
          (Web <$> webOpts)
          (Opt.progDesc "Run the equifuzz Web UI, connected to a remote host")
    , Opt.command "diy" $
        Opt.info
          (Diy <$> scFilePath <*> runnerConfigOpts)
          (Opt.progDesc "Attempt to construct and run an experiment from a SystemC file")
    , Opt.command "version" $
        Opt.info
          (pure PrintVersion)
          (Opt.progDesc "Print the software version")
    ]
 where
  webOpts = do
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
        }

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

  sshOpts = Opt.optional $ do
    host <-
      Opt.strOption . mconcat $
        [ Opt.long "host"
        , Opt.metavar "HOSTNAME"
        , Opt.help "Remote hostname or IP to run equivalence checker on"
        ]
    username <-
      Opt.strOption . mconcat $
        [ Opt.long "username"
        , Opt.metavar "USERNAME"
        , Opt.help "Username to connect to remote host"
        ]
    passwordSource <- askPasswordFlag <|> passwordOption <|> pure NoPassword
    activatePath <-
      optional . Opt.strOption . mconcat $
        [ Opt.long "activate-script"
        , Opt.metavar "PATH"
        , Opt.help "Script to be sourced on the remote host before running vcf"
        ]
    return SSHOptions{..}

  runnerConfigOpts = do
    sshOptions <- sshOpts
    ecConfig <-
      Opt.option readFecType . mconcat $
        [ Opt.long "fec-type"
        , Opt.metavar "TYPE"
        , Opt.help "What FEC type we are running against (vcf|catapult|jasper)"
        ]

    return RunnerOptions{sshOptions, ecConfig}

  askPasswordFlag :: Opt.Parser PasswordSource
  askPasswordFlag =
    Opt.flag' AskPassword . mconcat $
      [ Opt.long "ask-password"
      , Opt.help "Ask for SSH password to the remote host"
      ]

  passwordOption =
    PasswordGiven
      <$> ( Opt.strOption . mconcat $
              [ Opt.long "password"
              , Opt.metavar "PASSWORD"
              , Opt.help "Password to connect to remote host"
              ]
          )

  readFecType = Opt.eitherReader $ \case
    "vcf" -> Right vcFormal
    "jasper" -> Right jasper
    "slec" -> Right slec
    "test" -> Right testRunner
    other -> Left ("FEC '" ++ other ++ "' is unknown")

  scFilePath =
    Opt.argument Opt.str . mconcat $
      [ Opt.metavar "FILE"
      ]

parseArgs :: IO Command
parseArgs =
  Opt.execParser $
    Opt.info
      (commandParser <**> Opt.helper)
      ( mconcat
          [ Opt.fullDesc
          , Opt.progDesc "Fuzzer for formal equivalence checkers"
          ]
      )

--------------------------- Testing --------------------------------------------

testRunner :: EquivalenceCheckerConfig
testRunner =
  EquivalenceCheckerConfig
    { name = "test-runner"
    , runScript = "sleep 5; echo Test"
    , makeFiles = const []
    , parseOutput = \Experiment{experimentId} fullOutput ->
        ExperimentResult
          { proofFound = Just False
          , counterExample = Nothing
          , fullOutput
          , experimentId
          , extraInfos = Map.empty
          }
    , mods = noMods
    }

-- testRunner inconclusiveResults experiment = do
--   getStdRandom (uniformR (1_000_000, 5_000_000)) >>= threadDelay

--   proofFound <-
--     getStdRandom (uniformR (1 :: Int, 100)) <&> \x ->
--       if
--         | x < 10 && inconclusiveResults -> Nothing
--         | x < 20 -> Just (not experiment.expectedResult)
--         | otherwise -> Just experiment.expectedResult

--   let counterExample =
--         if proofFound == Just False
--           then Nothing
--           else Just "Counter-example goes here"
--   let fullOutput = "blah\nblah\nblah"

--   return
--     ExperimentResult
--       { experimentId = experiment.experimentId
--       , proofFound
--       , counterExample
--       , fullOutput
--       , extraInfos = Map.empty
--       }

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
            , transformationAllowed = runnerOptions.ecConfig.mods
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

runnerOptionsToRunner :: RunnerOptions -> IO ExperimentRunner
runnerOptionsToRunner
  RunnerOptions
    { sshOptions
    , ecConfig
    } = do
    -- host,
    -- username,
    -- passwordSource,
    -- activatePath,
    case sshOptions of
      Nothing -> return (runECLocal ecConfig)
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
        return $ runECRemote SSHConnectionTarget{..} activatePath ecConfig
