{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Orchestration (OrchestrationConfig (..), startRunners, startRunReduceThread) where

import Control.Concurrent (
  MVar,
  modifyMVar_,
  newEmptyMVar,
  newMVar,
  newQSem,
  putMVar,
  signalQSem,
  takeMVar,
  waitQSem,
 )
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.STM (
  TChan,
  atomically,
  cloneTChan,
  newTChanIO,
  readTChan,
  writeTChan,
 )
import Control.Exception (SomeException, bracket_, catch)
import Control.Monad (unless, void, when)
import Control.Monad.State (execStateT, liftIO)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Experiments (
  Experiment (..),
  ExperimentId (uuid),
  ExperimentResult (..),
  ExperimentSequenceId (uuid),
  genSystemCConstantExperiment,
  newExperimentSequenceId,
  saveExperimentWithResult,
 )
import GenSystemC (GenConfig)
import Optics (at, use)
import Optics.State.Operators ((.=))
import Reduce (HasReductions (..))
import Runners (ExperimentRunner)
import Shelly ((</>))
import Text.Printf (printf)
import Util (foreverThread, whenJust)
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
  runnerSem <- newQSem config.maxConcurrentExperiments

  let runner = bracket_ (waitQSem runnerSem) (signalQSem runnerSem) . rawRunner

  case config.experimentCount of
    Nothing -> forConcurrently_ [1 .. config.maxConcurrentExperiments] $ \idx -> do
      experimentVar <- newEmptyMVar
      foreverThread (printf "Producer %d" idx) $ do
        putMVar experimentVar =<< genSystemCConstantExperiment config.genConfig
      foreverThread (printf "Runner %d" idx) $ do
        takeMVar experimentVar >>= startRunReduceThread progressChan runner
    Just n -> forConcurrently_ [1 .. n] $ \idx -> do
      when config.verbose (printf "Runner %d started" idx)
      experimentReducible <- genSystemCConstantExperiment config.genConfig
      startRunReduceThread progressChan runner experimentReducible

reduceLoop :: (ExperimentProgress -> IO ()) -> ExperimentRunner -> ExperimentSequenceId -> Experiment -> IO Bool
reduceLoop progress runner sequenceId = go
 where
  go experiment = do
    progress (ExperimentStarted sequenceId experiment)
    result <-
      runner experiment
        `catch` \(err :: SomeException) -> pure (errorResult experiment.experimentId err)
    progress (ExperimentCompleted sequenceId result)
    -- is interesting?
    if result.proofFound /= Just experiment.expectedResult
      then reduceDepthFirst (mkReductions experiment) >> return True
      else return False

  reduceDepthFirst :: [IO Experiment] -> IO ()
  reduceDepthFirst [] = pure ()
  reduceDepthFirst (r : rs) = do
    experiment <- r
    isInteresting <- go experiment
    -- If this reduction is interesting, we're done. Dont' even try the next
    -- ones.
    unless isInteresting $ reduceDepthFirst rs

  errorResult experimentId err =
    ExperimentResult
      { experimentId = experimentId
      , proofFound = Nothing
      , counterExample = Nothing
      , fullOutput = "Runner crashed with exception:\n" <> T.pack (show err)
      , extraInfos = Map.empty
      }

startRunReduceThread :: ProgressChan -> ExperimentRunner -> Experiment -> IO ()
startRunReduceThread progressChan runner initialExperiment = do
  sequenceId <- newExperimentSequenceId

  void (reduceLoop progress runner sequenceId initialExperiment)
    `catch` ( \(err :: SomeException) -> do
                printf "Experiment sequence aborted %s\n" (show sequenceId)
                printf "==============================\n"
                printf "%s\n" (show err)
                printf "==============================\n\n"
            )
  progress (ExperimentSequenceCompleted sequenceId)
 where
  progress = atomically . writeTChan progressChan

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
            liftIO
              ( saveExperimentWithResult
                  (("experiments" :: String) </> UUID.toString experiment.experimentId.uuid)
                  experiment
                  result
              )
      ExperimentSequenceCompleted _ -> pure ()
