{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments.Runners (ExperimentRunner (..), allRunners, saveExperiment) where

import Control.Monad (forM_, void)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Experiments.Types
import Optics ((^.))
import Shelly ((</>))
import Shelly qualified as Sh
import Util (whenJust)

data ExperimentRunner = ExperimentRunner
  { info :: RunnerInfo,
    run :: Experiment -> IO (Either RunnerError ExperimentResult)
  }

allRunners :: [ExperimentRunner]
allRunners = [hector_2022_06_SP2, hector_2022_06_SP2_3, hector_2023_03_1]

hector_2022_06_SP2 :: ExperimentRunner
hector_2022_06_SP2 =
  ExperimentRunner
    { info,
      run =
        runVCFormal
          info
          "mp5617@ee-beholder0.ee.ic.ac.uk"
          "/scratch/mp5617/synopsys/vc_static/T-2022.06-SP2/activate.sh"
    }
  where
    info = "Hector T-2022.06-SP2"

hector_2022_06_SP2_3 :: ExperimentRunner
hector_2022_06_SP2_3 =
  ExperimentRunner
    { info,
      run =
        runVCFormal
          info
          "mp5617@ee-beholder0.ee.ic.ac.uk"
          "/scratch/mp5617/synopsys/vc_static/T-2022.06-SP2-3/activate.sh"
    }
  where
    info = "Hector T-2022.06-SP2-3"

hector_2023_03_1 :: ExperimentRunner
hector_2023_03_1 =
  ExperimentRunner
    { info,
      run =
        runVCFormal
          info
          "mp5617@ee-beholder0.ee.ic.ac.uk"
          "/scratch/mp5617/synopsys/vc_static/U-2023.03-1/activate.sh"
    }
  where
    info = "Hector T-2022.06-SP2-3"

-- | Run an experiment using VC Formal on a remote host
runVCFormal :: Text -> Text -> Text -> Experiment -> IO (Either RunnerError ExperimentResult)
runVCFormal runnerInfo vcfHost sourcePath experiment@Experiment {uuid, design} = Sh.shelly . Sh.silently $ do
  dir <- T.strip <$> Sh.run "mktemp" ["-d"]
  Sh.writefile
    (dir </> filename)
    design.source
  Sh.writefile
    (dir </> ("compare.tcl" :: Text))
    (hectorCompareScript filename experiment)
  void $ Sh.bash "ssh" [vcfHost, "mkdir -p " <> remoteDir <> "/"]
  void $ Sh.bash "scp" ["-r", dir <> "/*", vcfHost <> ":" <> remoteDir <> "/" <> T.pack (show uuid)]

  fullOutput <- Sh.silently $ Sh.run "ssh" [vcfHost, sshCommand]

  void . Sh.errExit False $
    Sh.bash
      "scp"
      [ vcfHost
          <> ":"
          <> remoteDir
          <> "/"
          <> T.pack (show uuid)
          <> "/counter_example.txt",
        dir <> "/counter_example.txt"
      ]

  void $ Sh.bash "ssh" [vcfHost, [i|"cd ~ && rm -rf ./#{experimentDir}"|]]

  counterExample <-
    Sh.test_f (T.unpack dir <> "/counter_example.txt") >>= \case
      False -> pure Nothing
      True -> Just <$> Sh.readfile (T.unpack dir <> "/counter_example.txt")

  let noLicense =
        fullOutput
          & T.lines
          & any ("Unable to check out license feature" `T.isInfixOf`)

  let proofSuccessful =
        fullOutput
          & T.lines
          & any ("Status for proof \"proof\": SUCCESSFUL" `T.isInfixOf`)

  let proofFailed =
        fullOutput
          & T.lines
          & any ("Status for proof \"proof\": FAILED" `T.isInfixOf`)

  let proofFound = case (proofSuccessful, proofFailed) of
        (True, False) -> Just True
        (False, True) -> Just False
        _ -> Nothing

  return $
    if noLicense
      then Left OutOfLicenses
      else Right ExperimentResult {proofFound, counterExample, fullOutput, uuid, runnerInfo}
  where
    remoteDir :: Text
    remoteDir = "equifuzz_vcf_experiment"

    experimentDir :: Text
    experimentDir = remoteDir <> "/" <> T.pack (show uuid)

    filename = "impl.cpp"

    sshCommand :: Text
    sshCommand = [i|cd #{experimentDir} && ls -ltr && md5sum *.v && source #{sourcePath} && vcf -fmode DPV -f compare.tcl|]

hectorCompareScript :: FilePath -> Experiment -> Text
hectorCompareScript filename experiment =
  [__i|
            set_custom_solve_script "orch_multipliers"
            set_user_assumes_lemmas_procedure "miter"

            create_design -name spec -top #{experiment ^. #design ^. #topName}
            cppan #{filename}
            compile_design spec

            create_design -name impl -top #{experiment ^. #design ^. #topName}
            cppan #{filename}
            compile_design impl

            proc miter {} {
                    map_by_name -inputs -implphase 1 -specphase 1
                    lemma out_equiv = spec.out(1) == #{experiment ^. #comparisonValue}
            }

            compose
            solveNB proof
            proofwait
            listproof
            simcex -txt counter_example.txt out_equiv
            quit
  |]

-- | Save information about the experiment to the experiments/ directory
saveExperiment :: Experiment -> [ExperimentResult] -> IO ()
saveExperiment experiment results = Sh.shelly . Sh.silently $ do
  let dir = "experiments/" <> UUID.toString experiment.uuid
  let filename = "impl.cpp"

  Sh.mkdir_p dir
  Sh.writefile (dir </> filename) experiment.design.source
  forM_ results $ \result -> do
    Sh.mkdir_p (dir </> result.runnerInfo)
    Sh.writefile
      (dir </> result.runnerInfo </> ("full_output.txt" :: Text))
      result.fullOutput

    whenJust result.counterExample $
      Sh.writefile
        (dir </> result.runnerInfo </> ("counter_example.txt" :: Text))

    Sh.writefile
      (dir </> result.runnerInfo </> ("info.txt" :: Text))
      [__i|
          Runner          : #{result ^. #runnerInfo}
          Proof Found     : #{result ^. #proofFound}
          Counter Example : #{isJust (result ^. #counterExample)}
          |]

  Sh.writefile (dir </> ("compare.tcl" :: Text)) (hectorCompareScript filename experiment)
