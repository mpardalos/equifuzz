{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}

module Main where

import Control.Applicative ((<**>))
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM (TBQueue, TChan, atomically, cloneTChan, newTBQueueIO, newTChanIO, readTBQueue, readTChan, writeTBQueue, writeTChan)
import Control.Concurrent.STM.TSem (newTSem, signalTSem, waitTSem)
import Control.Exception (SomeException, try)
import Control.Monad (forever, replicateM_, void)
import Data.Functor ((<&>))
import Data.Text.IO qualified as T
import Data.UUID.V4 qualified as UUID
import Experiments
import Options.Applicative qualified as Opt
import System.Random (getStdRandom, uniformR)
import Text.Printf (printf)
import Util (forkRestarting)
import WebUI (runWebUI)

type ExperimentQueue = TBQueue Experiment

type ProgressChan = TChan ExperimentProgress

newtype Config = Config
  { runner :: ExperimentRunner
  }

startGeneratorThread :: ExperimentQueue -> IO ()
startGeneratorThread queue = replicateM_ 10 . forkRestarting "Generator thread crashed" . forever $ do
  experiment <- mkSystemCConstantExperiment
  atomically (writeTBQueue queue experiment)

startOrchestratorThread :: Config -> ExperimentQueue -> ProgressChan -> IO ()
startOrchestratorThread Config {runner} experimentQueue progressChan = do
  maxExperiments <- atomically (newTSem 10)

  forkRestarting "Orchestrator thread crashed" $ forever $ do
    atomically (waitTSem maxExperiments)
    experiment <- atomically (readTBQueue experimentQueue)
    progress (NewExperiment experiment)
    forkFinally
      ( do
          progress (BeginRun experiment.uuid runner.info)
          result <- try @SomeException (runner.run experiment)

          case result of
            Left e -> progress (RunFailed experiment.uuid runner.info (RunnerCrashed e))
            Right (Left OutOfLicenses) -> do
              -- We have one less license than we thought, so reduce the max
              -- number of experiments allowed
              atomically (waitTSem maxExperiments)
              progress (RunFailed experiment.uuid runner.info OutOfLicenses)
            Right (Left e) -> progress (RunFailed experiment.uuid runner.info e)
            Right (Right r) -> progress (RunCompleted r)
      )
      ( \_ -> do
          atomically (signalTSem maxExperiments)
          progress (ExperimentCompleted experiment.uuid)
      )
  where
    progress = atomically . writeTChan progressChan

startLoggerThread :: ProgressChan -> IO ()
startLoggerThread progressChan =
  forkRestarting "Logger thread crashed" . forever $
    atomically (readTChan progressChan) >>= \case
      NewExperiment experiment -> printf "Begin experiment | %s\n" (show experiment.uuid)
      BeginRun uuid runnerInfo -> printf "Begin run | %s on %s\n" (show uuid) runnerInfo
      RunFailed uuid runnerInfo _ -> printf "Run failed | %s on %s\n" (show uuid) (show runnerInfo)
      RunCompleted result -> printf "Run completed | %s on %s\n" (show result.uuid) (result.runnerInfo)
      ExperimentCompleted uuid -> printf "Experiment Completed | %s\n" (show uuid)

webMain :: Bool -> IO ()
webMain test = do
  progressChan <- newTChanIO

  if test
    then startTestThread progressChan
    else do
      experimentQueue <- newTBQueueIO 20
      startGeneratorThread experimentQueue
      startOrchestratorThread config experimentQueue progressChan

  -- The following threads are readers and so we need to duplicate the progress
  -- channel so that they can all read the messages on it
  startLoggerThread =<< atomically (cloneTChan progressChan)
  runWebUI progressChan
  where
    config = Config {runner = hector_2023_03_1}

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

startTestThread :: ProgressChan -> IO ()
startTestThread progressChan = void . forkIO . forever . try @SomeException $ do
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

    reportProgress p = atomically (writeTChan progressChan p)
