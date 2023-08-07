{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}

module Orchestration (ProgressChan, OrchestrationConfig (..), startRunners) where

import Control.Concurrent (MVar, forkFinally, modifyMVar_, newMVar)
import Control.Concurrent.STM (TChan, atomically, cloneTChan, newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TSem (TSem, newTSem, signalTSem, waitTSem)
import Control.Exception (SomeException, catch)
import Control.Monad (void, when)
import Control.Monad.State (execStateT, liftIO)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Experiments
  ( Experiment (..),
    ExperimentId (uuid),
    ExperimentProgress (..),
    ExperimentResult (..),
    ExperimentRunner (..),
    ExperimentSequenceId (uuid),
    mkSystemCConstantExperiment,
    newExperimentSequenceId,
    saveExperiment,
  )
import GenSystemC (GenConfig, Reducible (..))
import Optics (at, use)
import Optics.State.Operators ((.=))
import Text.Printf (printf)
import Util (foreverThread, whenJust)

type ProgressChan = TChan ExperimentProgress

data OrchestrationConfig = OrchestrationConfig
  { runner :: ExperimentRunner,
    verbose :: Bool,
    saveResults :: Bool,
    generatorThreads :: Int,
    maxExperiments :: Int,
    experimentQueueDepth :: Int,
    genConfig :: GenConfig
  }

startRunners :: OrchestrationConfig -> IO ProgressChan
startRunners config = do
  progressChan <- newTChanIO

  startOrchestratorThread config config.runner progressChan

  when config.saveResults $
    startSaverThread =<< atomically (cloneTChan progressChan)

  when config.verbose $
    startLoggerThread =<< atomically (cloneTChan progressChan)

  return progressChan

startOrchestratorThread :: OrchestrationConfig -> ExperimentRunner -> ProgressChan -> IO ()
startOrchestratorThread config runner progressChan = do
  experimentSem <- atomically (newTSem . toInteger $ config.maxExperiments)

  foreverThread "Orchestrator" $ do
    atomically (waitTSem experimentSem)
    experimentReducible <- mkSystemCConstantExperiment config.genConfig
    startRunReduceThread experimentSem progressChan runner experimentReducible

startRunReduceThread :: TSem -> ProgressChan -> ExperimentRunner -> Reducible (IO Experiment) -> IO ()
startRunReduceThread experimentSem progressChan runner initialExperimentReducible = do
  sequenceId <- newExperimentSequenceId

  void $
    forkFinally
      (runReduceLoop sequenceId initialExperimentReducible)
      (const (endExperimentSequence sequenceId))
  where
    runReduceLoop :: ExperimentSequenceId -> Reducible (IO Experiment) -> IO ()
    runReduceLoop sequenceId experimentReducible = do
      experiment <- experimentReducible.value
      progress (ExperimentStarted sequenceId experiment)
      result <-
        runner.run experiment
          `catch` \(err :: SomeException) -> pure (errorResult experiment.experimentId err)
      progress (ExperimentCompleted sequenceId result)

    endExperimentSequence :: ExperimentSequenceId -> IO ()
    endExperimentSequence sequenceId = do
      atomically (signalTSem experimentSem)
      progress (ExperimentSequenceCompleted sequenceId)

    progress = atomically . writeTChan progressChan

    errorResult experimentId err =
      ExperimentResult
        { experimentId = experimentId,
          proofFound = Nothing,
          counterExample = Nothing,
          fullOutput = "Runner crashed with exception:\n" <> T.pack (show err)
        }

startLoggerThread :: ProgressChan -> IO ()
startLoggerThread progressChan =
  foreverThread "Logger" $
    atomically (readTChan progressChan) >>= \case
      ExperimentStarted sequenceId experiment -> printf "Experiment started | %s (in sequence %s)\n" (show experiment.experimentId.uuid) (show sequenceId.uuid)
      ExperimentCompleted sequenceId result -> printf "Experiment Completed | %s (in sequence %s)\n" (show result.experimentId.uuid) (show sequenceId.uuid)
      ExperimentSequenceCompleted sequenceId -> printf "Experiment Sequence Completed | %s\n" (show sequenceId.uuid)

startSaverThread :: ProgressChan -> IO ()
startSaverThread progressChan = do
  experimentResults :: MVar (Map ExperimentId Experiment) <- newMVar Map.empty

  foreverThread "Saver" $ do
    progress <- atomically (readTChan progressChan)
    modifyMVar_ experimentResults . execStateT $ case progress of
      ExperimentStarted _ experiment -> do
        at experiment.experimentId .= Just experiment
      ExperimentCompleted _ result -> do
        mExperiment <- use (at result.experimentId)
        whenJust mExperiment $ \experiment -> do
          when (result.proofFound /= Just experiment.expectedResult) $
            liftIO (saveExperiment experiment result)
      ExperimentSequenceCompleted _ -> pure ()
