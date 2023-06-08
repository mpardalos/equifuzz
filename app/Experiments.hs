{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Experiments
  ( experimentLoop,
    module Experiments.Runners,
    module Experiments.Generators,
    module Experiments.Types,
  )
where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM, forever, when)
import Data.Maybe (catMaybes)
import Experiments.Generators
import Experiments.Runners
import Experiments.Types

experimentLoop ::
  IO Experiment ->
  [ExperimentRunner] ->
  (ExperimentProgress -> IO ()) ->
  IO ()
experimentLoop generator runners progress = forever $ do
  -- FIXME: Handle errors from the generator
  experiment <- generator

  progress (NewExperiment experiment)

  results <- fmap catMaybes . forM runners $ \runner -> do
    progress (BeginRun experiment.uuid runner.info)
    Control.Exception.try @Control.Exception.SomeException (runner.run experiment) >>= \case
      Right (Right result) -> do
        progress (RunCompleted result)
        return (Just result)
      Right (Left runnerError) -> do
        progress (RunFailed experiment.uuid runner.info runnerError)
        -- Out of licenses *should* terminate the thread
        case runnerError of
          OutOfLicenses -> do
            progress (ExperimentCompleted experiment.uuid)
            throwIO OutOfLicenses
          _ -> return Nothing
      Left err -> do
        progress (RunFailed experiment.uuid runner.info (RunnerCrashed err))
        return Nothing

  let isInteresting result = case (experiment.expectedResult, result.proofFound) of
        (_, Nothing) -> True
        (True, Just False) -> True
        (False, Just True) -> True
        _ -> False

  when (any isInteresting results) $
    saveExperiment experiment results

  progress (ExperimentCompleted experiment.uuid)
