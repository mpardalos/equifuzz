{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad (void)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IO.Encoding
import Hedgehog.Gen qualified as Hog
import Optics
import Shelly ((</>))
import Shelly qualified as Sh
import Verismith.Generate (randomMod)
import Verismith.Verilog

vcfCompareScript :: Text -> Identifier -> Text -> Identifier -> Text
vcfCompareScript specfile spectop implfile impltop =
  [__i|
     set_custom_solve_script "orch_multipliers"
     set_user_assumes_lemmas_procedure "miter"

     create_design -name spec -top #{spectop ^. #getIdentifier} -lang mx
     vcs -sverilog #{specfile}
     compile_design spec

     create_design -name impl -top #{impltop^. #getIdentifier} -lang mx
     vcs -sverilog #{implfile}
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

data Experiment = Experiment
  { module1 :: ModDecl (),
    module2 :: ModDecl (),
    setupDir :: FilePath
  }

data ExperimentResult = ExperimentResult
  { proofFound :: Bool,
    fullOutput :: Text
  }

makeFieldLabelsNoPrefix ''Experiment

vcfHost :: Text
vcfHost = "ee-mill3"

sshCommand :: Text -> Text
sshCommand dir = [i|cd #{dir} && vcf -fmode DPV -f compare.tcl|]

runVCFormal :: Experiment -> IO ExperimentResult
runVCFormal (Experiment mod1 mod2 dir) = Sh.shelly $ do
  Sh.echo "Generating experiment"
  let file1 = mod1.id.getIdentifier <> ".v"
  let file2 = mod2.id.getIdentifier <> ".v"

  Sh.mkdir dir
  Sh.writefile (dir </> ("compare.tcl" :: Text)) (vcfCompareScript file1 mod1.id file2 mod2.id)
  Sh.writefile (dir </> file1) (genSource mod1)
  Sh.writefile (dir </> file2) (genSource mod2)
  Sh.echo "Copying files"
  void $ Sh.run "scp" ["-r", T.pack dir, vcfHost <> ":"]
  Sh.echo "Running experiment"
  fullOutput <- Sh.silently $ Sh.run "ssh" [vcfHost, sshCommand (T.pack dir)]
  let logFile = dir </> ("output.log" :: FilePath)
  Sh.echo ("Writing full output to " <> T.pack logFile)
  Sh.writefile logFile fullOutput
  let proofFound =
        fullOutput
          & T.lines
          & any ("Status for proof \"proof\": SUCCESSFUL" `T.isInfixOf`)
  return ExperimentResult {proofFound, fullOutput}

boxed :: Text -> Text -> Text
boxed title =
  T.unlines
    . (["╭───" <> title <> "───"] <>)
    . (<> ["╰──────"])
    . map ("│ " <>)
    . T.lines

main :: IO ()
main = do
  setLocaleEncoding utf8
  m <- Hog.sample (randomMod 2 4)
  result <- runVCFormal $
    Experiment
      (m & #id .~ Identifier "mod1")
      (m & #id .~ Identifier "mod2")
      "vcf_fuzz"
  if result.proofFound
    then putStrLn "Proof found. Modules are equivalent"
    else putStrLn "No proof found. Modules are non-equivalent"
