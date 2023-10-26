module Runners.Jasper (runJasper) where

import Data.Text (Text)
import Experiments.Types (Experiment, ExperimentResult)
import Runners.Types (SSHConnectionTarget)

runJasper :: SSHConnectionTarget -> Maybe Text -> Experiment -> IO ExperimentResult
runJasper = undefined
