module Testing (
  defConfig,
  someSystemC,
  someExperiment,
  writeExperiment,
  writeExperimentFor,
  module ToolRestrictions,
  module Runners,
  module Reduce,
  module GenSystemC,
  module Prettyprinter,
  Experiment (..),
)
where

import Data.Text qualified as T
import Experiments
import GenSystemC
import Prettyprinter
import Reduce
import Runners
import SystemC qualified as SC
import Text.Printf (printf)
import ToolRestrictions

defConfig :: GenConfig
defConfig =
  GenConfig
    { growSteps = 5
    , transformationAllowed = \_ _ -> True
    , evaluations = 3
    }

jasperConfig, vcfConfig, slecConfig :: GenConfig
jasperConfig = defConfig{transformationAllowed = jasperMods}
vcfConfig = defConfig{transformationAllowed = vcfMods}
slecConfig = defConfig{transformationAllowed = slecMods}

someSystemC :: IO SC.FunctionDeclaration
someSystemC = do
  process <- genSystemCProcess defConfig
  return (generateProcessToSystemC defConfig "top" process)

someExperimentWith :: GenConfig -> IO Experiment
someExperimentWith = genSystemCConstantExperiment

someExperiment :: IO Experiment
someExperiment = someExperimentWith defConfig

writeExperiment :: EquivalenceCheckerConfig -> Experiment -> IO ()
writeExperiment ec experiment = do
  let path = show experiment.experimentId.uuid
  createExperimentDir (T.pack path) (ec.makeFiles experiment)
  printf "Created %s\n" path

writeExperimentFor :: EquivalenceCheckerConfig -> IO ()
writeExperimentFor ec = do
  let cfg = case ec.name of
        "jasper" -> jasperConfig
        "vcf" -> vcfConfig
        "slec" -> slecConfig
        _ -> defConfig
  experiment <- someExperimentWith cfg
  let path = show experiment.experimentId.uuid
  createExperimentDir (T.pack path) (ec.makeFiles experiment)
  printf "Created %s\n" path
