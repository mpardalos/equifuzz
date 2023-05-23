{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments.Runners (ExperimentRunner(..), allRunners, saveExperiment) where

import Control.Monad (forM_, void)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Experiments.Types
import Optics ((^.))
import Shelly ((<.>), (</>))
import Shelly qualified as Sh
import Util (whenJust)

data ExperimentRunner = ExperimentRunner
  { info :: RunnerInfo,
    run :: Experiment -> IO ExperimentResult
  }

allRunners :: [ExperimentRunner]
allRunners = [hectorOnEEMill3, hectorOnEEBeholder0]

hectorOnEEMill3 :: ExperimentRunner
hectorOnEEMill3 =
  ExperimentRunner
    { info,
      run =
        runVCFormal
          info
          "mp5617@ee-mill3.ee.ic.ac.uk"
          "/eda/synopsys/2022-23/RHELx86/VC-STATIC_2022.06-SP2/bin/vcf"
    }
  where
    info = "Synopsys Hector on ee-mill3 (Version T-2022.06-SP2 for linux64 - Nov 29, 2022)"

hectorOnEEBeholder0 :: ExperimentRunner
hectorOnEEBeholder0 =
  ExperimentRunner
    { info,
      run =
        runVCFormal
          info
          "mp5617@ee-beholder0.ee.ic.ac.uk"
          "/home/mp5617/synopsys/vc_static/T-2022.06-SP2-3/bin/vcf"
    }
  where
    info = "Synopsys Hector on ee-beholder0 (Version T-2022.06-SP2-3 for linux64 - Apr 21, 2023)"

-- | Run an experiment using VC Formal on a remote host
runVCFormal :: Text -> Text -> Text -> Experiment -> IO ExperimentResult
runVCFormal runnerInfo vcfHost vcfPath experiment@Experiment {uuid, designSpec, designImpl} = Sh.shelly . Sh.silently $ do
  dir <- T.strip <$> Sh.run "mktemp" ["-d"]
  Sh.writefile
    (dir </> specFileName)
    designSpec.source
  Sh.writefile
    (dir </> implFileName)
    designImpl.source
  Sh.writefile
    (dir </> ("compare.tcl" :: Text))
    (hectorCompareScript specFileName implFileName experiment)
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

  return ExperimentResult {proofFound, counterExample, fullOutput, uuid, runnerInfo}
  where
    remoteDir :: Text
    remoteDir = "equifuzz_vcf_experiment"

    experimentDir :: Text
    experimentDir = remoteDir <> "/" <> T.pack (show uuid)

    specFileName, implFileName :: FilePath
    specFileName = ("spec" :: FilePath) <.> languageFileExtension designSpec.language
    implFileName = ("impl" :: FilePath) <.> languageFileExtension designImpl.language

    sshCommand :: Text
    sshCommand = [i|cd #{experimentDir} && ls -ltr && md5sum *.v && #{vcfPath} -fmode DPV -f compare.tcl|]

hectorCompareScript :: FilePath -> FilePath -> Experiment -> Text
hectorCompareScript specFileName implFileName experiment =
  [__i|
            set_custom_solve_script "orch_multipliers"
            set_user_assumes_lemmas_procedure "miter"

            create_design -name spec -top #{experiment ^. #designSpec ^. #topName}
            #{compileCommand (experiment ^. #designSpec ^. #language) specFileName}
            compile_design spec

            create_design -name impl -top #{experiment ^. #designImpl ^. #topName}
            #{compileCommand (experiment ^. #designImpl ^. #language) implFileName}
            compile_design impl

            proc miter {} {
                    map_by_name -inputs -implphase 1 -specphase 1
                    lemma out_equiv = spec.out(1) == impl.out(1)
            }

            compose
            solveNB proof
            proofwait
            listproof
            simcex -txt counter_example.txt out_equiv
            quit
  |]
  where
    compileCommand :: DesignLanguage -> FilePath -> FilePath
    compileCommand language file = case language of
      Verilog -> [i|vcs -sverilog #{file}|]
      SystemC -> [i|cppan #{file}|]

languageFileExtension :: DesignLanguage -> Text
languageFileExtension Verilog = "v"
languageFileExtension SystemC = "cpp"

-- | Save information about the experiment to the experiments/ directory
saveExperiment :: Experiment -> [ExperimentResult] -> IO ()
saveExperiment experiment results = Sh.shelly . Sh.silently $ do
  let dir = "experiments/" <> UUID.toString experiment.uuid
  let specFileName = ("spec" :: Text) <.> languageFileExtension experiment.designSpec.language
  let implFileName = ("impl" :: Text) <.> languageFileExtension experiment.designImpl.language

  Sh.mkdir_p dir
  Sh.writefile (dir </> specFileName) experiment.designSpec.source
  Sh.writefile (dir </> implFileName) experiment.designImpl.source
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

  Sh.writefile (dir </> ("compare.tcl" :: Text)) (hectorCompareScript specFileName implFileName experiment)
