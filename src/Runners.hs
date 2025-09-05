{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Runners (
  ExperimentRunner,
  EquivalenceCheckerConfig (..),
  SSHConnectionTarget (..),
  jasper,
  vcFormal,
  slec,
  testRunner,
  runECLocal,
  runECRemote,
  createExperimentDir,
  validateSSH,
)
where

import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments
import Optics ((%~), (&))
import Runners.Common
import Runners.Jasper
import Runners.SLEC
import Runners.Test
import Runners.VCF
import Shelly ((</>))
import Util (runBash)

runECRemote :: SSHConnectionTarget -> Maybe Text -> EquivalenceCheckerConfig -> Experiment -> IO ExperimentResult
runECRemote sshOpts mActivatePath EquivalenceCheckerConfig{..} experiment = do
  let baseDir = "equifuzz_" <> name <> "_experiment"
      remoteExperimentDir :: Text = T.pack (baseDir </> show experiment.experimentId.uuid)
      ecFiles = makeFiles experiment
  createRemoteExperimentDir sshOpts remoteExperimentDir ecFiles
  let runEC = case mActivatePath of
        Nothing -> runScript
        Just activatePath -> [i|source #{activatePath} && #{runScript}|]
  fullOutput <- runSSHCommand sshOpts [i|(cd #{remoteExperimentDir} && pwd && ls -ltrh && #{runEC}); rm -rf #{remoteExperimentDir}|]
  return
    ( parseOutput experiment fullOutput
        & #extraInfos
        %~ Map.union (Map.fromList ecFiles)
        & #extraInfos
        %~ Map.insert "Command" runEC
    )

runECLocal :: EquivalenceCheckerConfig -> Experiment -> IO ExperimentResult
runECLocal EquivalenceCheckerConfig{..} experiment = do
  let baseDir = "equifuzz_" <> name <> "_experiment"
      experimentDir :: Text = T.pack (baseDir </> show experiment.experimentId.uuid)
      ecFiles = makeFiles experiment
  createExperimentDir experimentDir ecFiles
  fullOutput <- runBash [i|(cd #{experimentDir} && pwd && ls -ltrh && #{runScript}); rm -rf #{experimentDir}|]
  return
    ( parseOutput experiment fullOutput
        & #extraInfos
        %~ Map.union (Map.fromList ecFiles)
        & #extraInfos
        %~ Map.insert "Command" runScript
    )
