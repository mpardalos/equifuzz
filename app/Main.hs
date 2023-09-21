{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Applicative (Alternative ((<|>)), optional, (<**>))
import Data.Text.IO qualified as T
import Experiments
import Runners
import GenSystemC (GenConfig (..))
import Options.Applicative qualified as Opt
import Orchestration
import WebUI (runWebUI)
#ifdef EVALUATION_VERSION
import Control.Monad.Random (setStdGen, mkStdGen)
#endif
import Meta (versionName)
import Optics (view)
import Control.Monad.Random (getStdRandom)
import System.Random (uniformR)
import Data.Functor ((<&>))
import Control.Concurrent (threadDelay)
import qualified SystemC as SC
import Data.Text (Text)
import System.IO (stdin, hSetEcho, hFlush, stdout)
import Text.Printf (printf)

main :: IO ()
main = do
#ifdef EVALUATION_VERSION
  -- We set a fixed seed for the evaluation version
  setStdGen (mkStdGen 120)
#endif
  parseArgs >>= \case
    Web webOpts -> do
      config <- webOptionsToOrchestrationConfig webOpts
      progressChan <- startRunners config
      runWebUI progressChan
    Generate genConfig -> do
      Experiment {design, longDescription, comparisonValue} <- mkSystemCConstantExperiment genConfig >>= view #value
      T.putStrLn (SC.genSource design)
      putStrLn "---------"
      T.putStrLn longDescription
      putStrLn "---------"
      T.putStrLn comparisonValue
    PrintVersion -> putStrLn versionName

--------------------------- CLI Parser -----------------------------------------

data Command
  = Web WebOptions
  | Generate GenConfig
  | PrintVersion

data WebOptions = WebOptions
  { verbose :: Bool,
    saveResults :: Bool,
    maxExperiments :: Int,
    runnerOptions :: RunnerOptions,
    genConfig :: GenConfig
  }

data PasswordSource = NoPassword | AskPassword | PasswordGiven Text

data FECType = VCF

data RunnerOptions
  = TestRunner
      { includeInconclusive :: Bool
      }
  | Runner
      { host :: Text,
        username :: Text,
        passwordSource :: PasswordSource,
        activatePath :: Maybe Text,
        fecType :: FECType
      }

webOptionsToOrchestrationConfig :: WebOptions -> IO OrchestrationConfig
webOptionsToOrchestrationConfig
  WebOptions
    { verbose,
      saveResults,
      maxExperiments,
      genConfig,
      runnerOptions
    } = do
    runner <- runnerOptionsToRunner runnerOptions
    return
      OrchestrationConfig
        { verbose,
          saveResults,
          maxExperiments,
          genConfig,
          runner
        }

runnerOptionsToRunner :: RunnerOptions -> IO ExperimentRunner
runnerOptionsToRunner TestRunner { includeInconclusive} =
  return (testRunner includeInconclusive)
runnerOptionsToRunner
  Runner
    { host,
      username,
      passwordSource,
      activatePath,
      fecType
    } = do
        password <- case passwordSource of
          NoPassword -> pure Nothing
          PasswordGiven pass -> pure (Just pass)
          AskPassword -> Just <$> askPassword
        return $ ExperimentRunner $ case fecType of
          VCF -> runVCFormal SSHConnectionTarget {username, host, password} activatePath

askPassword :: IO Text
askPassword = do
    printf "Your password: "
    hFlush stdout
    hSetEcho stdin False
    password <- T.getLine
    hSetEcho stdin True
    return password

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "web" $
        Opt.info
          (Web <$> webOpts)
          (Opt.progDesc "Run the equifuzz Web UI, connected to a remote host"),
      Opt.command "generate" $
        Opt.info
          (Generate <$> generateConfigOpts)
          (Opt.progDesc "Generate a sample of a generator"),
      Opt.command "version" $
        Opt.info
          (pure PrintVersion)
          (Opt.progDesc "Print the software version")
    ]
  where
    webOpts = do
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
      saveResults <- not <$> (Opt.switch . mconcat $
          [ Opt.long "no-save",
            Opt.help "Do not save successful experiment results"
          ])
      genConfig <- generateConfigOpts
      runnerOptions <- runnerConfigOpts <|> testFlag
      return
        WebOptions
          { verbose,
            saveResults,
            maxExperiments,
            runnerOptions,
            genConfig
          }

    testFlag :: Opt.Parser RunnerOptions
    testFlag = do
      Opt.flag' () . mconcat $
        [ Opt.long "test",
          Opt.help "Use a 'test' runner, that just gives random results (for testing)"
        ]

      includeInconclusive <-
        Opt.switch . mconcat $
          [ Opt.long "inconclusive",
            Opt.help "Include inconclusive results in the test results"
          ]

      return TestRunner {includeInconclusive}

    generateConfigOpts :: Opt.Parser GenConfig
    generateConfigOpts =
#ifdef EVALUATION_VERSION
      pure (GenConfig 20)
#else
      GenConfig
        <$> ( Opt.option Opt.auto . mconcat $
                [ Opt.long "gen-steps",
                  Opt.metavar "COUNT",
                  Opt.help "Number of generation steps to use for each experiment",
                  Opt.value 30,
                  Opt.showDefault
                ]
            )
#endif

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
      passwordSource <- askPasswordFlag <|> passwordOption <|> pure NoPassword
      activatePath <-
        optional . Opt.strOption . mconcat $
          [ Opt.long "activate-script",
            Opt.metavar "PATH",
            Opt.help "Script to be sourced on the remote host before running vcf"
          ]

      fecType <- Opt.option readFecType . mconcat $
        [ Opt.long "fec-type"
        , Opt.metavar "TYPE"
        , Opt.help "What FEC type we are running against (vcf|catapult|jasper)"
        ]

      return Runner {host, username, passwordSource, activatePath, fecType}

    askPasswordFlag :: Opt.Parser PasswordSource
    askPasswordFlag =
      Opt.flag' AskPassword . mconcat $
        [ Opt.long "ask-password",
          Opt.help "Ask for SSH password to the remote host"
        ]

    passwordOption =
      PasswordGiven
        <$> ( Opt.strOption . mconcat $
                [ Opt.long "password",
                  Opt.metavar "PASSWORD",
                  Opt.help "Password to connect to remote host"
                ]
            )

    readFecType = Opt.eitherReader $ \case
      "vcf" -> Right VCF
      "catapult" -> Left "Siemens catapult is not *yet* supported"
      "jasper" -> Left "Cadence jasper is not *yet* supported"
      other -> Left ("FEC '" ++ other ++ "' is unknown")

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

--------------------------- Testing --------------------------------------------

testRunner :: Bool -> ExperimentRunner
testRunner inconclusiveResults = ExperimentRunner $ \experiment -> do
  getStdRandom (uniformR (1_000_000, 5_000_000)) >>= threadDelay

  proofFound <-
    getStdRandom (uniformR (1 :: Int, 100)) <&> \x -> if
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
      { experimentId = experiment.experimentId,
        proofFound,
        counterExample,
        fullOutput
      }
