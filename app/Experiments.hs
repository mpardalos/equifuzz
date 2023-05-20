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

import Control.Exception (SomeException, try)
import Control.Monad (forM, forever, when)
import Data.Either (rights)
import Data.Text qualified as T
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
  results <- fmap rights . forM runners $ \runner -> do
    progress (BeginRun experiment.uuid runner.info)
    Control.Exception.try @Control.Exception.SomeException (runner.run experiment) >>= \case
      Right result -> do
        progress (RunCompleted result)
        return (Right result)
      Left err -> do
        progress
          ( RunCompleted
              ExperimentResult
                { proofFound = Nothing,
                  counterExample = Nothing,
                  fullOutput = T.pack (show err),
                  runnerInfo = runner.info,
                  uuid = experiment.uuid
                }
          )
        return (Left err)

  let isInteresting result = case (experiment.expectedResult, result.proofFound) of
        (_, Nothing) -> True
        (True, Just False) -> True
        (False, Just True) -> True
        _ -> False

  when (any isInteresting results) $
    saveExperiment experiment results

  progress (ExperimentCompleted experiment.uuid)
