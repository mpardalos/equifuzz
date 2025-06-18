{-# LANGUAGE QuasiQuotes #-}

module Experiments (
  module Experiments.Generators,
  module Experiments.Types,
  saveExperiment,
)
where

import Data.Maybe (isJust)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.UUID qualified as UUID
import Experiments.Generators
import Experiments.Types
import Optics ((^.))
import Shelly ((</>))
import Shelly qualified as Sh
import SystemC qualified as SC
import Util (whenJust)

-- | Save information about the experiment to the experiments/ directory
saveExperiment :: Experiment -> ExperimentResult -> IO ()
saveExperiment experiment result = Sh.shelly . Sh.silently $ do
  let localExperimentDir = "experiments/" <> UUID.toString experiment.experimentId.uuid

  Sh.mkdir_p localExperimentDir
  Sh.writefile (localExperimentDir </> ("dut.cpp" :: Text)) (SC.genSource experiment.design)
  Sh.writefile (localExperimentDir </> ("description.txt" :: Text)) experiment.longDescription

  Sh.writefile
    (localExperimentDir </> ("full_output.txt" :: Text))
    result.fullOutput

  whenJust result.counterExample $
    Sh.writefile
      (localExperimentDir </> ("counter_example.txt" :: Text))

  Sh.writefile
    (localExperimentDir </> ("info.txt" :: Text))
    [__i|
        Proof Found     : #{result ^. #proofFound}
        Counter Example : #{isJust (result ^. #counterExample)}
        |]
