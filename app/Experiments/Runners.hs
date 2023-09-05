{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments.Runners
  ( ExperimentRunner (..),
    SSHConnectionTarget (..),
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
import Optics (makeFieldLabelsNoPrefix, (^.))
import Shelly (Sh, (</>))
import Shelly qualified as Sh
import Util (whenJust)

newtype ExperimentRunner = ExperimentRunner
  { run :: Experiment -> IO ExperimentResult
  }

data SSHConnectionTarget = SSHConnectionTarget
  { host :: Text,
    username :: Text,
    password :: Maybe Text
  }

makeFieldLabelsNoPrefix ''SSHConnectionTarget

bashExec :: Text -> Sh Text
bashExec commands = Sh.run "bash" ["-c", commands]

bashExec_ :: Text -> Sh ()
bashExec_ = void . bashExec

-- | Run an experiment using VC Formal on a remote host
runVCFormal :: SSHConnectionTarget -> Maybe Text -> Experiment -> IO ExperimentResult
runVCFormal sshOpts mSourcePath experiment@Experiment {experimentId, design} = Sh.shelly . Sh.silently $ do
  let sshString = sshOpts.username <> "@" <> sshOpts.host
  let ssh :: Text = case sshOpts.password of
        Nothing -> "ssh -o PasswordAuthentication=no -o StrictHostKeychecking=no"
        Just pass -> [i|sshpass -p #{pass} ssh -o StrictHostKeychecking=no|]
  let scp :: Text = case sshOpts.password of
        Nothing -> "scp -o PasswordAuthentication=no -o StrictHostKeychecking=no"
        Just pass -> [i|sshpass -p #{pass} scp -o StrictHostKeychecking=no|]

  localExperimentDir <- T.strip <$> Sh.run "mktemp" ["-d"]

  Sh.writefile
    (localExperimentDir </> filename)
    design.source
  Sh.writefile
    (localExperimentDir </> ("compare.tcl" :: Text))
    (hectorCompareScript filename experiment)
  bashExec_
    [i|#{ssh} #{sshString} mkdir -p #{remoteExperimentDir}/ |]
  bashExec_
    [i|#{scp} -r #{localExperimentDir}/* #{sshString}:#{remoteExperimentDir}/|]

  fullOutput <- bashExec [i|#{ssh} #{sshString} '#{sshCommand}'|]

  void . Sh.errExit False $
    bashExec [i|#{scp} #{sshString}:#{remoteExperimentDir}/counter_example.txt #{localExperimentDir}/counter_example.txt|]

  bashExec_ [i|#{ssh} #{sshString} 'cd ~ && rm -rf ./#{remoteExperimentDir}'|]

  counterExample <-
    Sh.test_f (T.unpack localExperimentDir <> "/counter_example.txt") >>= \case
      False -> pure Nothing
      True -> Just <$> Sh.readfile (T.unpack localExperimentDir <> "/counter_example.txt")

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

  return $ ExperimentResult {proofFound, counterExample, fullOutput, experimentId}
  where
    remoteDir :: Text
    remoteDir = "equifuzz_vcf_experiment"

    remoteExperimentDir :: Text
    remoteExperimentDir = [i|#{remoteDir}/#{experimentId ^. #uuid}|]

    filename = "impl.cpp"

    sshCommand :: Text
    sshCommand = case mSourcePath of
      Just sourcePath -> [i|cd #{remoteExperimentDir} && ls -ltr && md5sum * && source #{sourcePath} && vcf -fmode DPV -f compare.tcl|]
      Nothing -> [i|cd #{remoteExperimentDir} && ls -ltr && md5sum * && vcf -fmode DPV -f compare.tcl|]

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
  let localExperimentDir = "experiments/" <> UUID.toString experiment.experimentId.uuid
  let filename = "impl.cpp"

  Sh.mkdir_p localExperimentDir
  Sh.writefile (localExperimentDir </> filename) experiment.design.source
  Sh.writefile (localExperimentDir </> ("description.txt" :: Text)) experiment.designDescription

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

  Sh.writefile (localExperimentDir </> ("compare.tcl" :: Text)) (hectorCompareScript filename experiment)
