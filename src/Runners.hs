{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Runners (
  module Runners.Types,
  module Runners.Jasper,
  module Runners.VCF,
  module Runners.SLEC,
  runECLocal,
  runECRemote
)
where

import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments.Types (Experiment (..), ExperimentId (..), ExperimentResult)
import Runners.Jasper
import Runners.SLEC
import Runners.Types
import Runners.Util (createExperimentDir, createRemoteExperimentDir, runSSHCommand)
import Runners.VCF
import Shelly (liftIO, shelly, (</>))
import Util (runBash)

runECRemote :: SSHConnectionTarget -> Maybe Text -> EquivalenceCheckerConfig -> Experiment -> IO ExperimentResult
runECRemote sshOpts mActivatePath EquivalenceCheckerConfig{..} experiment = do
  let baseDir = "equifuzz_" <> name <> "_experiment"
      remoteExperimentDir :: Text = T.pack (baseDir </> show experiment.experimentId.uuid)
  fullOutput <- shelly $ do
    createRemoteExperimentDir sshOpts remoteExperimentDir (makeFiles experiment)
    let runEC = case mActivatePath of
          Nothing -> runScript
          Just activatePath -> [i|source #{activatePath} && #{runScript}|]
    runSSHCommand sshOpts [i|cd #{remoteExperimentDir} && pwd && ls -ltrh && md5sum * && #{runEC}|]
  return (parseOutput experiment fullOutput)

runECLocal :: EquivalenceCheckerConfig -> Experiment -> IO ExperimentResult
runECLocal EquivalenceCheckerConfig{..} experiment = do
  let baseDir = "equifuzz_" <> name <> "_experiment"
      experimentDir :: Text = T.pack (baseDir </> show experiment.experimentId.uuid)
  fullOutput <- shelly $ do
    createExperimentDir experimentDir (makeFiles experiment)
    liftIO $ runBash [i|cd #{experimentDir} && pwd && ls -ltrh && md5sum * && #{runScript}|]
  return (parseOutput experiment fullOutput)
