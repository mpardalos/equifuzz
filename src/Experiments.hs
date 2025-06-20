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
import SystemC qualified as SC
import Util (whenJust, mkdir_p)
import qualified Data.Text.IO as TIO

-- | Save information about the experiment to the experiments/ directory
saveExperiment :: Experiment -> ExperimentResult -> IO ()
saveExperiment experiment result = do
  let localExperimentDir = "experiments/" <> UUID.toText experiment.experimentId.uuid

  mkdir_p localExperimentDir
  TIO.writeFile (localExperimentDir </> ("dut.cpp" :: Text)) (SC.genSource experiment.design)
  TIO.writeFile (localExperimentDir </> ("description.txt" :: Text)) experiment.longDescription

  TIO.writeFile
    (localExperimentDir </> ("full_output.txt" :: Text))
    result.fullOutput

  whenJust result.counterExample $
    TIO.writeFile
      (localExperimentDir </> ("counter_example.txt" :: Text))

  TIO.writeFile
    (localExperimentDir </> ("info.txt" :: Text))
    [__i|
        Proof Found     : #{result ^. #proofFound}
        Counter Example : #{isJust (result ^. #counterExample)}
        |]
