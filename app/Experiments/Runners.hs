{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments.Runners
  ( ExperimentRunner (..),
    runVCFormal,
    saveExperiment,
  )
where

import Control.Monad (void)
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

newtype ExperimentRunner = ExperimentRunner
  { run :: Experiment -> IO (Either RunnerError ExperimentResult)
  }

-- | Run an experiment using VC Formal on a remote host
runVCFormal :: Text -> Text -> Maybe Text -> Experiment -> IO (Either RunnerError ExperimentResult)
runVCFormal username vcfHost mSourcePath experiment@Experiment {uuid, design} = Sh.shelly . Sh.silently $ do
  let sshString = username <> "@" <> vcfHost

  dir <- T.strip <$> Sh.run "mktemp" ["-d"]
  Sh.writefile
    (dir </> filename)
    design.source
  Sh.writefile
    (dir </> ("compare.tcl" :: Text))
    (hectorCompareScript filename experiment)
  void $ Sh.bash "ssh" [sshString, "mkdir -p " <> remoteDir <> "/"]
  void $ Sh.bash "scp" ["-r", dir <> "/*", sshString <> ":" <> remoteDir <> "/" <> T.pack (show uuid)]

  fullOutput <- Sh.run "ssh" [sshString, sshCommand]

  void . Sh.errExit False $
    Sh.bash
      "scp"
      [ sshString
          <> ":"
          <> remoteDir
          <> "/"
          <> T.pack (show uuid)
          <> "/counter_example.txt",
        dir <> "/counter_example.txt"
      ]

  void $ Sh.bash "ssh" [sshString, [i|"cd ~ && rm -rf ./#{experimentDir}"|]]

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
      else Right ExperimentResult {proofFound, counterExample, fullOutput, uuid}
  where
    remoteDir :: Text
    remoteDir = "equifuzz_vcf_experiment"

    experimentDir :: Text
    experimentDir = remoteDir <> "/" <> T.pack (show uuid)

    filename = "impl.cpp"

    sshCommand :: Text
    sshCommand = case mSourcePath of
      Just sourcePath -> [i|cd #{experimentDir} && ls -ltr && md5sum * && source #{sourcePath} && vcf -fmode DPV -f compare.tcl|]
      Nothing -> [i|cd #{experimentDir} && ls -ltr && md5sum * && vcf -fmode DPV -f compare.tcl|]

hectorCompareScript :: FilePath -> Experiment -> Text
hectorCompareScript filename experiment =
  [__i|
            set_custom_solve_script "orch_multipliers"
            set_user_assumes_lemmas_procedure "miter"

            create_design -name impl -top #{experiment ^. #design ^. #topName}
            scdtan -DSC_INCLUDE_FX #{filename}
            compile_design impl

            proc miter {} {
                    lemma out_equiv = out(1) == #{experiment ^. #comparisonValue}
            }

            compose -nospec
            solveNB proof
            proofwait
            listproof
            simcex -txt counter_example.txt out_equiv
            exit
            exit
  |]

-- | Save information about the experiment to the experiments/ directory
saveExperiment :: Experiment -> ExperimentResult -> IO ()
saveExperiment experiment result = Sh.shelly . Sh.silently $ do
  let dir = "experiments/" <> UUID.toString experiment.uuid
  let filename = "impl.cpp"

  Sh.mkdir_p dir
  Sh.writefile (dir </> filename) experiment.design.source
  Sh.writefile (dir </> ("description.txt" :: Text)) experiment.designDescription

  Sh.writefile
    (dir </> ("full_output.txt" :: Text))
    result.fullOutput

  whenJust result.counterExample $
    Sh.writefile
      (dir </> ("counter_example.txt" :: Text))

  Sh.writefile
    (dir </> ("info.txt" :: Text))
    [__i|
        Proof Found     : #{result ^. #proofFound}
        Counter Example : #{isJust (result ^. #counterExample)}
        |]

  Sh.writefile (dir </> ("compare.tcl" :: Text)) (hectorCompareScript filename experiment)
