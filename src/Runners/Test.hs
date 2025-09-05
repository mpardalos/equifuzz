module Runners.Test where

import Data.Map qualified as Map
import Experiments
import Runners.Common
import ToolRestrictions (noMods)

testRunner :: EquivalenceCheckerConfig
testRunner =
  EquivalenceCheckerConfig
    { name = "test-runner"
    , runScript = "sleep 5; echo Test"
    , makeFiles = const []
    , parseOutput = \Experiment{experimentId} fullOutput ->
        ExperimentResult
          { proofFound = Just False
          , counterExample = Nothing
          , fullOutput
          , experimentId
          , extraInfos = Map.empty
          }
    , mods = noMods
    }

-- testRunner inconclusiveResults experiment = do
--   getStdRandom (uniformR (1_000_000, 5_000_000)) >>= threadDelay

--   proofFound <-
--     getStdRandom (uniformR (1 :: Int, 100)) <&> \x ->
--       if
--         | x < 10 && inconclusiveResults -> Nothing
--         | x < 20 -> Just (not experiment.expectedResult)
--         | otherwise -> Just experiment.expectedResult

--   let counterExample =
--         if proofFound == Just False
--           then Nothing
--           else Just "Counter-example goes here"
--   let fullOutput = "blah\nblah\nblah"

--   return
--     ExperimentResult
--       { experimentId = experiment.experimentId
--       , proofFound
--       , counterExample
--       , fullOutput
--       , extraInfos = Map.empty
--       }
