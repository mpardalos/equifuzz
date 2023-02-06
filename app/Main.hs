{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad (void)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IO.Encoding
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog
import Optics
import Shelly ((</>))
import Shelly qualified as Sh
import Transform (applySPTransformation, doubleInvertCondition, or0, possibly)
import Verismith.Generate (randomMod)
import Verismith.Verilog

data Experiment = Experiment
  { module1 :: ModDecl (),
    module2 :: ModDecl (),
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
  let file1 = mod1.id.getIdentifier <> ".v"
  let file2 = mod2.id.getIdentifier <> ".v"

  Sh.mkdir dir
  Sh.writefile (dir </> ("compare.tcl" :: Text)) (compareScript file1 mod1.id file2 mod2.id)
  Sh.writefile (dir </> file1) (genSource mod1)
  Sh.writefile (dir </> file2) (genSource mod2)
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
    compareScript :: Text -> Identifier -> Text -> Identifier -> Text
    compareScript specfile spectop implfile impltop =
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

    vcfHost :: Text
    vcfHost = "ee-mill3"

    sshCommand :: Text
    sshCommand = [i|cd #{dir} && vcf -fmode DPV -f compare.tcl|]

boxed :: Text -> Text -> Text
boxed title =
  T.unlines
    . (["╭───" <> title <> "───"] <>)
    . (<> ["╰──────"])
    . map ("│ " <>)
    . T.lines

-- | Pick some random semantics-preserving transformations and apply them
randomizeSP :: Data a => a -> Gen a
randomizeSP val = do
  transformations <-
    Hog.list (Hog.linear 1 10)
      . Hog.element
      . map (fmap (transformBiM . possibly))
      $ [doubleInvertCondition, or0]
  foldrM applySPTransformation val transformations

main :: IO ()
main = do
  setLocaleEncoding utf8
  m1 :: ModDecl () <- Hog.sample (randomMod 2 4)
  m2 <- Hog.sample (randomizeSP m1)
  result <-
    runVCFormal $
      Experiment
        (m1 & #id .~ Identifier "mod1")
        (m2 & #id .~ Identifier "mod2")
        "vcf_fuzz"
  if result.proofFound
    then putStrLn "Proof found. Modules are equivalent"
    else putStrLn "No proof found. Modules are non-equivalent"
