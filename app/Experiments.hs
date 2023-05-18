{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

  progress (Began experiment)
  results <- fmap rights . forM runners $ \runner -> do
    Control.Exception.try @Control.Exception.SomeException $ runner experiment

  let isInteresting result = case (experiment.expectedResult, result.proofFound) of
        (_, Nothing) -> True
        (True, Just False) -> True
        (False, Just True) -> True
        _ -> False

  when (any isInteresting results) $
    saveExperiment experiment results

  mapM_ (progress . Completed) results
