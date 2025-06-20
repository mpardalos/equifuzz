{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# HLINT ignore "Use unless" #-}
{-# LANGUAGE RecordWildCards #-}

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
import Optics (view, isn't, _Right, (%), only)
import Control.Monad.Random (getStdRandom)
import System.Random (uniformR)
import Data.Functor ((<&>))
import Control.Concurrent (threadDelay)
import qualified SystemC as SC
import Data.Text (Text)
import System.IO (stdin, hSetEcho, hFlush, stdout)
import Text.Printf (printf)
import Control.Monad (replicateM_, when, forM_)
import Options.Applicative (ReadM)
import Runners.Util (validateSSH)
import Control.Exception (throwIO, try, SomeException)
import ToolRestrictions (vcfMods, noMods, jasperMods, slecMods)
import GenSystemC.Config (GenMods)
import qualified Data.Text as T

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
    Generate genOpts -> do
      replicateM_ genOpts.count $ do
        Experiment {design, longDescription, knownEvaluations} <-
          mkSystemCConstantExperiment (generateOptionsToGenConfig genOpts) >>= view #value
        T.putStrLn (SC.genSource design)
        putStrLn "---------"
        T.putStrLn longDescription
        putStrLn "---------"
        forM_ knownEvaluations $ \Evaluation{inputs, output} -> do
          T.putStr "\n*\t"
          T.putStr $ T.intercalate "\n\t" $
            [
              name <> "=" <> comparisonValueAsC value
              | ((_, name), value) <- zip design.args inputs
            ]
          T.putStrLn ""
          T.putStr "\t-> "
          T.putStr (comparisonValueAsC output)
    PrintVersion -> putStrLn versionName

--------------------------- CLI Parser -----------------------------------------

data Command
  = Web WebOptions
  | Generate GenerateOptions
  | PrintVersion

data WebOptions = WebOptions
  { verbose :: Bool,
    saveResults :: Bool,
    maxConcurrentExperiments :: Int,
    experimentCount :: Maybe Int,
    runnerOptions :: RunnerOptions,
    genSteps :: Int,
    evaluations :: Int
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

generateOptionsToGenConfig :: GenerateOptions -> GenConfig
generateOptionsToGenConfig GenerateOptions {genSteps, evaluations} =
  GenConfig
    { growSteps = genSteps
    , mods = noMods
    , evaluations
    }

webOptionsToOrchestrationConfig :: WebOptions -> IO OrchestrationConfig
webOptionsToOrchestrationConfig
  WebOptions
    { verbose,
      saveResults,
      maxConcurrentExperiments,
      experimentCount,
      genSteps,
      runnerOptions,
      evaluations
    } = do
    runner <- runnerOptionsToRunner runnerOptions
    let genConfig =
          GenConfig
            { growSteps = genSteps
            , mods = runnerOptionsToGenMods runnerOptions
            , evaluations
            }
    return
      OrchestrationConfig
        { verbose,
          saveResults,
          maxConcurrentExperiments,
          experimentCount,
          genConfig,
          runner
        }

runnerOptionsToGenMods :: RunnerOptions -> GenMods
runnerOptionsToGenMods Runner { fecType = VCF } = vcfMods
runnerOptionsToGenMods Runner { fecType = Jasper } = jasperMods
runnerOptionsToGenMods Runner { fecType = SLEC } = slecMods
runnerOptionsToGenMods TestRunner {} = noMods

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
        return $ runECRemote SSHConnectionTarget {..} activatePath ec

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
          (Opt.progDesc "Run the equifuzz Web UI, connected to a remote host"),
      Opt.command "generate" $
        Opt.info
          (Generate <$> generateOpts)
          (Opt.progDesc "Generate a sample of a generator"),
      Opt.command "version" $
        Opt.info
          (pure PrintVersion)
          (Opt.progDesc "Print the software version")
    ]
  where
    webOpts = do
      maxConcurrentExperiments <-
        Opt.option Opt.auto . mconcat $
          [ Opt.long "max-concurrent",
            Opt.metavar "COUNT",
            Opt.help "Maximum number of experiments to allow to run concurrently",
            Opt.value 10,
            Opt.showDefault
          ]
      experimentCount <-
        Opt.optional . Opt.option Opt.auto . mconcat $
          [ Opt.long "experiment-count",
            Opt.metavar "COUNT",
            Opt.help "Only run this many experiments",
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
      evaluations <- evaluationsFlag
      genSteps <- genStepsFlag
      runnerOptions <- runnerConfigOpts <|> testFlag
      return
        WebOptions
          { verbose,
            saveResults,
            maxConcurrentExperiments,
            experimentCount,
            runnerOptions,
            genSteps,
            evaluations
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

    generateOpts :: Opt.Parser GenerateOptions
    generateOpts = do
      count <-
        Opt.option (Opt.auto >>= validateCount) . mconcat $
          [ Opt.long "count",
            Opt.metavar "COUNT",
            Opt.help "How many examples to generate",
            Opt.value 1,
            Opt.showDefault
          ]

      evaluations <- evaluationsFlag
      genSteps <- genStepsFlag
      return GenerateOptions {count, genSteps, evaluations}

    validateCount :: Int -> ReadM Int
#ifdef EVALUATION_VERSION
    validateCount n
      | n > 100 = Opt.readerError "Cannot generate more than 100 examples in evaluation version"
      | otherwise = pure n
#else
    validateCount = pure
#endif

    evaluationsFlag :: Opt.Parser Int
    evaluationsFlag =
      Opt.option Opt.auto . mconcat $
        [ Opt.long "evaluations",
          Opt.metavar "COUNT",
          Opt.help "How many inputs to evaluate the generated design at",
          Opt.value 10,
          Opt.showDefault
        ]

    genStepsFlag :: Opt.Parser Int
    genStepsFlag =
#ifdef EVALUATION_VERSION
      pure 20
#else
      Opt.option Opt.auto . mconcat $
        [ Opt.long "gen-steps",
          Opt.metavar "COUNT",
          Opt.help "Number of generation steps to use for each experiment",
          Opt.value 30,
          Opt.showDefault
        ]
#endif

    sshOpts = Opt.optional $ do
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
      return SSHOptions {..}

    runnerConfigOpts = do
      sshOptions <- sshOpts
      fecType <- Opt.option readFecType . mconcat $
        [ Opt.long "fec-type"
        , Opt.metavar "TYPE"
        , Opt.help "What FEC type we are running against (vcf|catapult|jasper)"
        ]

      return Runner {sshOptions, fecType}

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
      "jasper" -> Right Jasper
      "slec" -> Right SLEC
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
testRunner inconclusiveResults experiment = do
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
