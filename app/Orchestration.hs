{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}

module Orchestration (ProgressChan, OrchestrationConfig (..), RunnerConfig (..), startRunners) where

import Control.Concurrent (MVar, forkFinally, forkIO, modifyMVar_, newMVar, threadDelay)
import Control.Concurrent.STM (TChan, atomically, cloneTChan, newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TSem (newTSem, signalTSem, waitTSem)
import Control.Exception (SomeException, try)
import Control.Monad (forever, replicateM_, void, when)
import Control.Monad.State (execStateT, liftIO)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Experiments (DesignSource (..), Experiment (..), ExperimentId, ExperimentProgress (..), ExperimentResult (..), ExperimentRunner (..), RunnerError (..), mkSystemCConstantExperiment, newExperimentId, newExperimentSequenceId, saveExperiment)
import GenSystemC (GenConfig, Reducible (..))
import Optics (at, use)
import Optics.State.Operators ((.=))
import System.Random (getStdRandom, uniformR)
import Text.Printf (printf)
import Util (foreverThread, whenJust)

type ProgressChan = TChan ExperimentProgress

data RunnerConfig
  = TestRunner
  | RunnerConfig ExperimentRunner

data OrchestrationConfig = OrchestrationConfig
  { runnerConfig :: RunnerConfig,
    verbose :: Bool,
    generatorThreads :: Int,
    maxExperiments :: Int,
    experimentQueueDepth :: Int,
    genConfig :: GenConfig
  }

startRunners :: OrchestrationConfig -> IO ProgressChan
startRunners config = do
  progressChan <- newTChanIO

  case config.runnerConfig of
    TestRunner ->
      replicateM_ config.maxExperiments $ startTestThread progressChan
    RunnerConfig runner -> do
      startOrchestratorThread config runner progressChan
      -- We need to clone the progressChan because, otherwise, this thread would
      -- "eat up" all the messages on it. This way it just gets a copy
      startSaverThread =<< atomically (cloneTChan progressChan)

  when config.verbose $
    startLoggerThread =<< atomically (cloneTChan progressChan)

  return progressChan

startOrchestratorThread :: OrchestrationConfig -> ExperimentRunner -> ProgressChan -> IO ()
startOrchestratorThread config runner progressChan = do
  experimentSem <- atomically (newTSem . toInteger $ config.maxExperiments)

  foreverThread "Orchestrator" $ do
    atomically (waitTSem experimentSem)

    sequenceId <- newExperimentSequenceId
    experimentReducible <- mkSystemCConstantExperiment config.genConfig
    experiment <- experimentReducible.value

    forkFinally
      ( do
          progress (ExperimentStarted sequenceId experiment)
          result :: Either RunnerError ExperimentResult <- try @RunnerError (runner.run experiment)

          case result of
            Right r -> progress (ExperimentCompleted r)
            (Left (RunnerCrashed e)) ->
              progress (ExperimentCompleted (errorResult experiment.experimentId (RunnerCrashed e)))
            (Left OutOfLicenses) -> do
              -- We have one less license than we thought, so reduce the max
              -- number of experiments allowed
              atomically (waitTSem experimentSem)
              progress (ExperimentCompleted (errorResult experiment.experimentId OutOfLicenses))
      )
      ( \result -> do
          atomically (signalTSem experimentSem)
          case result of
            Left e -> progress (ExperimentCompleted (errorResult experiment.experimentId (RunnerCrashed e)))
            Right () -> pure ()
          progress (ExperimentSequenceCompleted sequenceId)
      )
  where
    progress = atomically . writeTChan progressChan

    errorResult experimentId err =
      ExperimentResult
        { experimentId = experimentId,
          proofFound = Nothing,
          counterExample = Nothing,
          fullOutput = "",
          runnerError = Just err
        }

startLoggerThread :: ProgressChan -> IO ()
startLoggerThread progressChan =
  foreverThread "Logger" $
    atomically (readTChan progressChan) >>= \case
      ExperimentStarted sequenceId experiment -> printf "Experiment started | %s (in sequence %s)\n" (show experiment.experimentId) (show sequenceId)
      ExperimentCompleted result -> printf "Experiment Completed | %s\n" (show result.experimentId)
      ExperimentSequenceCompleted sequenceId -> printf "Experiment Sequence Completed | %s\n" (show sequenceId)

startSaverThread :: ProgressChan -> IO ()
startSaverThread progressChan = do
  experimentResults :: MVar (Map ExperimentId Experiment) <- newMVar Map.empty

  foreverThread "Saver" $ do
    progress <- atomically (readTChan progressChan)
    modifyMVar_ experimentResults . execStateT $ case progress of
      ExperimentStarted _ experiment -> do
        at experiment.experimentId .= Just experiment
      ExperimentCompleted result -> do
        mExperiment <- use (at result.experimentId)
        whenJust mExperiment $ \experiment -> do
          when (result.proofFound /= Just experiment.expectedResult) $
            liftIO (saveExperiment experiment result)
      ExperimentSequenceCompleted _ -> pure ()

--------------------------- Testing --------------------------------------------

startTestThread :: ProgressChan -> IO ()
startTestThread progressChan = void . forkIO . forever . try @SomeException $ do
  sequenceId <- newExperimentSequenceId
  experiment <- mkTestExperiment

  threadDelay =<< getStdRandom (uniformR (1e6, 2e6))

  proofFound <-
    getStdRandom (uniformR (1 :: Int, 100)) <&> \x ->
      if
          | x < 10 -> Nothing
          | x < 20 -> Just True
          | otherwise -> Just False

  reportProgress (ExperimentStarted sequenceId experiment)

  threadDelay =<< getStdRandom (uniformR (5e6, 20e6))
  reportProgress
    ( ExperimentCompleted
        ExperimentResult
          { proofFound,
            counterExample = Just "counter example goes here",
            fullOutput = "blah\nblah\nblah",
            experimentId = experiment.experimentId,
            runnerError = Nothing
          }
    )
  where
    mkTestExperiment :: IO Experiment
    mkTestExperiment = do
      experimentId <- newExperimentId
      return
        Experiment
          { experimentId,
            design =
              DesignSource
                { topName = "main",
                  source = "int main() { return 0; }"
                },
            comparisonValue = "32'hdeadbeef",
            expectedResult = False,
            designDescription = "Mock experiments"
          }

    reportProgress p = atomically (writeTChan progressChan p)
