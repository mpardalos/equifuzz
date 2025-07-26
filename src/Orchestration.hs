{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Orchestration (OrchestrationConfig (..), startRunners) where

import Control.Concurrent (MVar, QSem, forkFinally, modifyMVar_, newMVar, newQSem, signalQSem, threadDelay, waitQSem)
import Control.Concurrent.STM (TChan, atomically, cloneTChan, newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TSem (TSem, newTSem, signalTSem, waitTSem)
import Control.Exception (SomeException, bracket_, catch)
import Control.Monad (forever, replicateM_, void, when)
import Control.Monad.State (execStateT, liftIO)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Experiments (
  Experiment (..),
  ExperimentId (uuid),
  ExperimentResult (..),
  ExperimentSequenceId (uuid),
  genSystemCConstantExperiment,
  newExperimentSequenceId,
  saveExperiment,
 )
import GenSystemC (GenConfig)
import Optics (at, use)
import Optics.State.Operators ((.=))
import Reduce (HasReductions (..))
import Runners (ExperimentRunner)
import Text.Printf (printf)
import Util (foldMUntil_, foreverThread, whenJust)
import WebUI (ExperimentProgress (..))

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
startOrchestratorThread config rawRunner progressChan = do
  experimentSem <- newQSem (2 * config.maxConcurrentExperiments)
  runnerSem <- newQSem config.maxConcurrentExperiments

  let runner = bracket_ (waitQSem runnerSem) (signalQSem runnerSem) . rawRunner

  case config.experimentCount of
    Nothing -> foreverThread "Orchestrator" $ do
      experimentReducible <- genSystemCConstantExperiment config.genConfig
      startRunReduceThread experimentSem progressChan runner experimentReducible
    Just n -> foreverThread "Orchestrator" $ do
      replicateM_ n $ do
        experimentReducible <- genSystemCConstantExperiment config.genConfig
        startRunReduceThread experimentSem progressChan runner experimentReducible
      when config.verbose (putStrLn "All required experiments started")
      forever (threadDelay maxBound)

startRunReduceThread :: QSem -> ProgressChan -> ExperimentRunner -> Experiment -> IO ()
startRunReduceThread experimentSem progressChan runner initialExperiment = do
  sequenceId <- newExperimentSequenceId

  waitQSem experimentSem
  void $
    forkFinally
      (runReduceLoop sequenceId initialExperiment)
      ( \result -> do
          signalQSem experimentSem
          progress (ExperimentSequenceCompleted sequenceId)
          case result of
            Right _ -> pure ()
            Left err -> do
              printf "Experiment sequence aborted %s\n" (show sequenceId)
              printf "==============================\n"
              printf "%s\n" (show err)
              printf "==============================\n\n"
      )
 where
  runReduceLoop :: ExperimentSequenceId -> Experiment -> IO Bool
  runReduceLoop sequenceId experiment =
    ( do
        progress (ExperimentStarted sequenceId experiment)
        result <-
          runner experiment
            `catch` \(err :: SomeException) -> pure (errorResult experiment.experimentId err)
        progress (ExperimentCompleted sequenceId result)

        let isInteresting = result.proofFound /= Just experiment.expectedResult

        when isInteresting $
          void $
            foldMUntil_
              (\genExperiment -> genExperiment >>= runReduceLoop sequenceId)
              (mkReductions experiment)

        return isInteresting
    )
      `catch` ( \(err :: SomeException) -> do
                  printf "Experiment failed in sequence %s\n" (show sequenceId)
                  printf "==============================\n"
                  printf "%s\n" (show err)
                  printf "==============================\n\n"
                  return False
              )

  progress = atomically . writeTChan progressChan

  errorResult experimentId err =
    ExperimentResult
      { experimentId = experimentId
      , proofFound = Nothing
      , counterExample = Nothing
      , fullOutput = "Runner crashed with exception:\n" <> T.pack (show err)
      , extraInfos = Map.empty
      }

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
