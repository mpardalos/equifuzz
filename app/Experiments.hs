{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments where

import Control.Monad (void)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Optics (makeFieldLabelsNoPrefix, (&), (^.))
import Shelly ((</>))
import Shelly qualified as Sh
import Verismith.Verilog (SourceInfo(..), genSource)
import Verismith.Verilog.AST (Annotation (..), Identifier (..), ModDecl (..))

data Experiment = forall ann1 ann2.
  (Show (AnnModDecl ann1), Show (AnnModDecl ann2)) =>
  Experiment
  { design1 :: SourceInfo ann1,
    design2 :: SourceInfo ann2,
    setupDir :: FilePath
  }

makeFieldLabelsNoPrefix ''Experiment

data ExperimentResult = ExperimentResult
  { proofFound :: Bool,
    fullOutput :: Text
  }

makeFieldLabelsNoPrefix ''ExperimentResult

runVCFormal :: Experiment -> IO ExperimentResult
runVCFormal (Experiment design1 design2 dir) = Sh.shelly $ do
  Sh.echo "Generating experiment"

  Sh.mkdir dir
  Sh.writefile (dir </> ("compare.tcl" :: Text)) (compareScript "design1.v" design1.top "design2.v" design2.top)
  Sh.writefile (dir </> ("design1.v" :: Text)) (genSource design1)
  Sh.writefile (dir </> ("design2.v" :: Text)) (genSource design2)
  Sh.echo "Copying files"
  void $ Sh.run "scp" ["-r", T.pack dir, vcfHost <> ":"]
  Sh.echo "Running experiment"
  fullOutput <- Sh.silently $ Sh.run "ssh" [vcfHost, sshCommand]
  let logFile = dir </> ("output.log" :: FilePath)
  Sh.echo ("Writing full output to " <> T.pack logFile)
  Sh.writefile logFile fullOutput
  let proofFound =
        fullOutput
          & T.lines
          & any ("Status for proof \"proof\": SUCCESSFUL" `T.isInfixOf`)
  return ExperimentResult {proofFound, fullOutput}
  where
    compareScript :: Text -> Text -> Text -> Text -> Text
    compareScript file1 top1 file2 top2 =
      [__i|
                set_custom_solve_script "orch_multipliers"
                set_user_assumes_lemmas_procedure "miter"

                create_design -name spec -top #{top1} -lang mx
                vcs -sverilog #{file1}
                compile_design spec

                create_design -name impl -top #{top2} -lang mx
                vcs -sverilog #{file2}
                compile_design impl

                proc miter {} {
                        map_by_name -inputs -implphase 1 -specphase 1
                        map_by_name -outputs -implphase 1 -specphase 1
                }

                compose
                solveNB proof
                proofwait
                listproof
                quit
    |]

    vcfHost :: Text
    vcfHost = "ee-mill3"

    sshCommand :: Text
    sshCommand = [i|cd #{dir} && vcf -fmode DPV -f compare.tcl|]

saveExperiment :: Experiment -> IO ()
saveExperiment (Experiment mod1 mod2 dir) = Sh.shelly $ do
  Sh.echo "Generating experiment"
  Sh.mkdir dir
  Sh.writefile (dir </> ("design1.v" :: FilePath)) (genSource mod1)
  Sh.writefile (dir </> ("design2.v" :: FilePath)) (genSource mod2)
