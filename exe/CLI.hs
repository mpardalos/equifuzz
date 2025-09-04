{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use unless" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module CLI where

import Control.Applicative (Alternative ((<|>)), optional, (<**>))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as T
import Experiments
import Options.Applicative qualified as Opt
import Runners
import System.IO (hFlush, hSetEcho, stdin, stdout)
import Text.Printf (printf)
import ToolRestrictions (noMods)

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
