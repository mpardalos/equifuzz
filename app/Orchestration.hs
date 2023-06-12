{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}

module Orchestration (ProgressChan, OrchestrationConfig (..), startRunners) where

import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM (TBQueue, TChan, atomically, cloneTChan, newTBQueueIO, newTChanIO, readTBQueue, readTChan, writeTBQueue, writeTChan)
import Control.Concurrent.STM.TSem (newTSem, signalTSem, waitTSem)
import Control.Exception (SomeException, try)
import Control.Monad (forever, replicateM_, void, when)
import Data.Functor ((<&>))
import Data.UUID.V4 qualified as UUID
import Experiments (DesignSource (..), Experiment (..), ExperimentProgress (..), ExperimentResult (..), ExperimentRunner (..), RunnerError (..), mkSystemCConstantExperiment)
import System.Random (getStdRandom, uniformR)
import Text.Printf (printf)
import Util (forkRestarting)

type ExperimentQueue = TBQueue Experiment

type ProgressChan = TChan ExperimentProgress

data OrchestrationConfig = OrchestrationConfig
  { runner :: ExperimentRunner,
    test :: Bool,
    logging :: Bool,
    generatorThreads :: Int,
    maxExperiments :: Int,
    experimentQueueDepth :: Int
  }

startRunners :: OrchestrationConfig -> IO ProgressChan
startRunners config = do
  progressChan <- newTChanIO

  if config.test
    then do
      startTestThread progressChan
    else do
      experimentQueue <- newTBQueueIO (fromIntegral config.experimentQueueDepth)
      startGeneratorThread config experimentQueue
      startOrchestratorThread config experimentQueue progressChan

  when config.logging $
    -- We need to clone the progressChan because, otherwise, this thread would
    -- "eat up" all the messages on it. This way it just gets a copy
    startLoggerThread =<< atomically (cloneTChan progressChan)

  return progressChan

startGeneratorThread :: OrchestrationConfig -> ExperimentQueue -> IO ()
startGeneratorThread OrchestrationConfig {generatorThreads} queue = replicateM_ generatorThreads
  . forkRestarting "Generator thread crashed"
  . forever
  $ do
    experiment <- mkSystemCConstantExperiment
    atomically (writeTBQueue queue experiment)

startOrchestratorThread :: OrchestrationConfig -> ExperimentQueue -> ProgressChan -> IO ()
startOrchestratorThread OrchestrationConfig {runner, maxExperiments} experimentQueue progressChan = do
  experimentSem <- atomically (newTSem . toInteger $ maxExperiments)

  forkRestarting "Orchestrator thread crashed" $ forever $ do
    atomically (waitTSem experimentSem)
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
              atomically (waitTSem experimentSem)
              progress (RunFailed experiment.uuid runner.info OutOfLicenses)
            Right (Left e) -> progress (RunFailed experiment.uuid runner.info e)
            Right (Right r) -> progress (RunCompleted r)
      )
      ( \_ -> do
          atomically (signalTSem experimentSem)
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
