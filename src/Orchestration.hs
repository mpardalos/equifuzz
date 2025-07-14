{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Orchestration (ProgressChan, OrchestrationConfig (..), startRunners) where

import Control.Concurrent (MVar, forkFinally, modifyMVar_, newMVar, threadDelay)
import Control.Concurrent.STM (TChan, atomically, cloneTChan, newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TSem (TSem, newTSem, signalTSem, waitTSem)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, replicateM_, void, when)
import Control.Monad.State (execStateT, liftIO)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Experiments (
  Experiment (..),
  ExperimentId (uuid),
  ExperimentProgress (..),
  ExperimentResult (..),
  ExperimentSequenceId (uuid),
  mkSystemCConstantExperiment,
  newExperimentSequenceId,
  saveExperiment,
 )
import GenSystemC (GenConfig, Reducible (..))
import Optics (at, use)
import Optics.State.Operators ((.=))
import Runners (ExperimentRunner)
import Text.Printf (printf)
import Util (foldMUntil_, foreverThread, whenJust)

type ProgressChan = TChan ExperimentProgress

data OrchestrationConfig = OrchestrationConfig
  { runner :: ExperimentRunner
  , verbose :: Bool
  , saveResults :: Bool
  , maxConcurrentExperiments :: Int
  , experimentCount :: Maybe Int
  , genConfig :: GenConfig
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
  experimentSem <- atomically (newTSem . toInteger $ config.maxConcurrentExperiments)
  case config.experimentCount of
    Nothing -> foreverThread "Orchestrator" $ do
      experimentReducible <- mkSystemCConstantExperiment config.genConfig
      startRunReduceThread experimentSem progressChan runner experimentReducible
    Just n -> foreverThread "Orchestrator" $ do
      replicateM_ n $ do
        experimentReducible <- mkSystemCConstantExperiment config.genConfig
        startRunReduceThread experimentSem progressChan runner experimentReducible
      when config.verbose (putStrLn "All required experiments started")
      forever (threadDelay maxBound)

startRunReduceThread :: TSem -> ProgressChan -> ExperimentRunner -> Reducible (IO Experiment) -> IO ()
startRunReduceThread experimentSem progressChan runner initialExperimentReducible = do
  sequenceId <- newExperimentSequenceId

  atomically (waitTSem experimentSem)
  void $
    forkFinally
      (runReduceLoop sequenceId initialExperimentReducible)
      ( \case
          Right _ -> endExperimentSequence sequenceId
          Left err -> do
            printf "Experiment sequence aborted %s\n" (show sequenceId)
            printf "==============================\n"
            printf "%s\n" (show err)
            printf "==============================\n\n"
            endExperimentSequence sequenceId
      )
 where
  runReduceLoop :: ExperimentSequenceId -> Reducible (IO Experiment) -> IO Bool
  runReduceLoop sequenceId experimentReducible = do
    experiment <- experimentReducible.value
    progress (ExperimentStarted sequenceId experiment)
    result <-
      runner experiment
        `catch` \(err :: SomeException) -> pure (errorResult experiment.experimentId err)
    progress (ExperimentCompleted sequenceId result)

    let isInteresting = result.proofFound /= Just experiment.expectedResult

    when (isInteresting && experimentReducible.size > 1) $
      void $
        foldMUntil_
          (runReduceLoop sequenceId)
          (selectReductions experimentReducible)

    return isInteresting

  endExperimentSequence :: ExperimentSequenceId -> IO ()
  endExperimentSequence sequenceId = do
    atomically (signalTSem experimentSem)
    progress (ExperimentSequenceCompleted sequenceId)

  progress = atomically . writeTChan progressChan

  errorResult experimentId err =
    ExperimentResult
      { experimentId = experimentId
      , proofFound = Nothing
      , counterExample = Nothing
      , fullOutput = "Runner crashed with exception:\n" <> T.pack (show err)
      }

selectReductions :: Reducible a -> [Reducible a]
selectReductions Reducible{reductions, size} =
  mapMaybe (reductions Map.!?) $
    [ (start, end)
    | chunks <- [2 .. size]
    , let chunkSize = size `div` chunks
    , chunkSize > 1
    , start <- [0, chunkSize .. size - chunkSize]
    , let end = start + chunkSize - 1
    ]
      <> [(n, n) | n <- [0 .. size - 1]]

startLoggerThread :: ProgressChan -> IO ()
startLoggerThread progressChan =
  foreverThread "Logger" $
    atomically (readTChan progressChan) >>= \case
      ExperimentStarted sequenceId experiment ->
        printf "Experiment started | %s (in sequence %s)\n" (show experiment.experimentId.uuid) (show sequenceId.uuid)
      ExperimentCompleted sequenceId result ->
        printf "Experiment Completed | %s (in sequence %s)\n" (show result.experimentId.uuid) (show sequenceId.uuid)
      ExperimentSequenceCompleted sequenceId ->
        printf "Experiment Sequence Completed | %s\n" (show sequenceId.uuid)

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
