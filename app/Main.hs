{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative ((<**>))
import Control.Concurrent (forkFinally, forkIO, newMVar, threadDelay)
import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (SomeException, fromException, try)
import Control.Monad (forever, replicateM_, void)
import Data.Functor ((<&>))
import Data.Text.IO qualified as T
import Data.Time (ZonedTime (zonedTimeToLocalTime), getZonedTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID.V4 qualified as UUID
import Experiments
import Options.Applicative qualified as Opt
import System.Random (getStdRandom, uniformR)
import Text.Printf (printf)
import WebUI (handleProgress, newWebUIState, runWebUI)

errorLog :: FilePath
errorLog = "equifuzz.error.log"

reportError :: String -> String -> IO ()
reportError title err = do
  time <- zonedTimeToLocalTime <$> getZonedTime
  appendFile errorLog . unlines $
    [ printf "[%s] %s" (iso8601Show time) title,
      "=========================",
      err,
      "=========================",
      ""
    ]

forkRestarting :: String -> IO () -> IO ()
forkRestarting title action =
  void $
    forkFinally
      action
      ( \err -> do
          reportError title (show err)
          forkRestarting title action
      )

startGeneratorThread :: TBQueue Experiment -> IO ()
startGeneratorThread queue = do
  forkRestarting "Generator thread crashed" $ forever $ do
    experiment <- mkSystemCConstantExperiment
    atomically (writeTBQueue queue experiment)
  return queue

startExperimentThread :: TBQueue Experiment -> (ExperimentProgress -> IO ()) -> IO ()
startExperimentThread experimentQueue reportProgress =
  void $
    forkFinally
      ( experimentLoop
          (atomically (readTBQueue experimentQueue))
          allRunners
          reportProgress
      )
      ( \err -> do
          reportError "Experiment thread crashed" (show err)
          case err of
            -- Let the runner just end if we are out of licenses
            Left (fromException -> Just OutOfLicenses) -> pure ()
            _ -> startExperimentThread experimentQueue reportProgress
      )

webMain :: Bool -> IO ()
webMain test = do
  experimentQueue <- newTBQueueIO 20
  stateVar <- newMVar =<< newWebUIState

  let reportProgress p = do
        case p of
          NewExperiment experiment -> printf "Begin experiment | %s\n" (show experiment.uuid)
          BeginRun uuid runnerInfo -> printf "Begin run | %s on %s\n" (show uuid) runnerInfo
          RunFailed uuid runnerInfo _ -> printf "Run failed | %s on %s\n" (show uuid) (show runnerInfo)
          RunCompleted result -> printf "Run completed | %s on %s\n" (show result.uuid) (result.runnerInfo)
          ExperimentCompleted uuid -> printf "Experiment Completed | %s\n" (show uuid)
        handleProgress stateVar p

  startGeneratorThread experimentQueue

  replicateM_ 10 $
    if test
      then testThread reportProgress
      else startExperimentThread experimentQueue reportProgress

  runWebUI stateVar

genMain :: IO ()
genMain = do
  Experiment {design, comparisonValue} <- mkSystemCConstantExperiment
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

--------------------------- Testing --------------------------------------------

testThread :: (ExperimentProgress -> IO ()) -> IO ()
testThread reportProgress = void . forkIO . forever . try @SomeException $ do
  experiment <- mkTestExperiment
  reportProgress (NewExperiment experiment)

  threadDelay =<< getStdRandom (uniformR (1e6, 2e6))

  proofFound <-
    getStdRandom (uniformR (1 :: Int, 100)) <&> \x ->
      if
          | x < 10 -> Nothing
          | x < 20 -> Just True
          | otherwise -> Just False

  reportProgress (BeginRun experiment.uuid "test-runner-1")
  reportProgress (BeginRun experiment.uuid "test-runner-2")

  threadDelay =<< getStdRandom (uniformR (5e6, 20e6))
  reportProgress
    ( RunCompleted
        ExperimentResult
          { proofFound,
            runnerInfo = "test-runner-1",
            counterExample = Just "counter example goes here",
            fullOutput = "blah\nblah\nblah",
            uuid = experiment.uuid
          }
    )

  threadDelay =<< getStdRandom (uniformR (5e6, 20e6))
  reportProgress
    ( RunCompleted
        ExperimentResult
          { proofFound,
            runnerInfo = "test-runner-2",
            counterExample = Just "counter example goes here",
            fullOutput = "blah\nblah\nblah",
            uuid = experiment.uuid
          }
    )

  reportProgress (ExperimentCompleted experiment.uuid)
  where
    mkTestExperiment :: IO Experiment
    mkTestExperiment = do
      uuid <- UUID.nextRandom
      return
        Experiment
          { uuid,
            design =
              DesignSource
                { topName = "main",
                  source = "int main() { return 0; }"
                },
            comparisonValue = "32'hdeadbeef",
            expectedResult = False
          }
