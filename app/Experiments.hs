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
import Verismith.Verilog (genSource)
import Verismith.Verilog.AST (Annotation (..), Identifier (..), ModDecl (..))

data Experiment = forall ann1 ann2.
  (Show (AnnModDecl ann1), Show (AnnModDecl ann2)) =>
  Experiment
  { module1 :: ModDecl ann1,
    module2 :: ModDecl ann2,
    setupDir :: FilePath
  }

makeFieldLabelsNoPrefix ''Experiment

data ExperimentResult = ExperimentResult
  { proofFound :: Bool,
    fullOutput :: Text
  }

makeFieldLabelsNoPrefix ''ExperimentResult

runVCFormal :: Experiment -> IO ExperimentResult
runVCFormal (Experiment mod1 mod2 dir) = Sh.shelly $ do
  Sh.echo "Generating experiment"

  Sh.mkdir dir
  Sh.writefile (dir </> ("compare.tcl" :: Text)) (compareScript "modules.v" mod1.id mod2.id)
  Sh.writefile (dir </> ("modules.v" :: Text)) (genSource mod1 <> "\n\n" <> genSource mod2)
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
    compareScript :: Text -> Identifier -> Identifier -> Text
    compareScript srcfile spectop impltop =
      [__i|
                set_custom_solve_script "orch_multipliers"
                set_user_assumes_lemmas_procedure "miter"

                create_design -name spec -top #{spectop ^. #getIdentifier} -lang mx
                vcs -sverilog #{srcfile}
                compile_design spec

                create_design -name impl -top #{impltop^. #getIdentifier} -lang mx
                vcs -sverilog #{srcfile}
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
  let file1 = mod1.id.getIdentifier <> ".v"
  let file2 = mod2.id.getIdentifier <> ".v"

  Sh.mkdir dir
  Sh.writefile (dir </> file1) (genSource mod1)
  Sh.writefile (dir </> file2) (genSource mod2)
