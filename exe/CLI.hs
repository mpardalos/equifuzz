{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# HLINT ignore "Use unless" #-}

module CLI where

import Control.Applicative (Alternative ((<|>)), optional, (<**>))
import Data.Text (Text)
import Data.Text.IO qualified as T
import Options.Applicative qualified as Opt
import System.IO (hFlush, hSetEcho, stdin, stdout)
import Text.Printf (printf)

data Command
  = Web WebOptions
  | Generate GenerateOptions
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

data GenerateOptions = GenerateOptions
  { count :: Int
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

data RunnerOptions
  = TestRunner {includeInconclusive :: Bool}
  | Runner
      { sshOptions :: Maybe SSHOptions
      , fecType :: FECType
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
    , Opt.command "generate" $
        Opt.info
          (Generate <$> generateOpts)
          (Opt.progDesc "Generate a sample of a generator")
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
    runnerOptions <- runnerConfigOpts <|> testFlag
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

  testFlag :: Opt.Parser RunnerOptions
  testFlag = do
    Opt.flag' () . mconcat $
      [ Opt.long "test"
      , Opt.help "Use a 'test' runner, that just gives random results (for testing)"
      ]

    includeInconclusive <-
      Opt.switch . mconcat $
        [ Opt.long "inconclusive"
        , Opt.help "Include inconclusive results in the test results"
        ]

    return TestRunner{includeInconclusive}

  generateOpts :: Opt.Parser GenerateOptions
  generateOpts = do
    count <-
      Opt.option Opt.auto . mconcat $
        [ Opt.long "count"
        , Opt.metavar "COUNT"
        , Opt.help "How many examples to generate"
        , Opt.value 1
        , Opt.showDefault
        ]

    evaluations <- evaluationsFlag
    genSteps <- genStepsFlag
    return GenerateOptions{count, genSteps, evaluations}

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
    fecType <-
      Opt.option readFecType . mconcat $
        [ Opt.long "fec-type"
        , Opt.metavar "TYPE"
        , Opt.help "What FEC type we are running against (vcf|catapult|jasper)"
        ]

    return Runner{sshOptions, fecType}

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
    "vcf" -> Right VCF
    "jasper" -> Right Jasper
    "slec" -> Right SLEC
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
