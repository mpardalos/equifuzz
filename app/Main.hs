{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad (void)
import Data.Data (Data)
import Data.Foldable (foldrM)
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
import Transform
  ( AnnTransform,
    annotateForTransformations,
    anywherePossibly,
    applyNSPTransformation,
    applySPTransformation,
    exprTransformsNSP,
    exprTransformsSP,
    somewhereReachable,
  )
import Verismith.Generate as Generate
  ( ConfProperty (..),
    Config (..),
    ProbExpr (..),
    ProbMod (..),
    ProbModItem (..),
    ProbStatement (..),
    Probability (..),
    randomMod,
  )
import Verismith.Verilog
import Verismith.Verilog.AST (Annotation (..))

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

boxed :: Text -> Text -> Text
boxed title =
  T.unlines
    . (["╭───" <> title <> "───"] <>)
    . (<> ["╰──────"])
    . map ("│ " <>)
    . T.lines

genConfig :: Generate.Config
genConfig =
  Config
    { probability =
        Probability
          { modItem =
              ProbModItem
                { assign = 2,
                  seqAlways = 0,
                  combAlways = 1,
                  inst = 0
                },
            stmnt =
              ProbStatement
                { block = 0,
                  nonBlock = 3,
                  cond = 1,
                  for = 0
                },
            expr =
              ProbExpr
                { num = 1,
                  id = 5,
                  rangeSelect = 5,
                  unOp = 5,
                  binOp = 5,
                  cond = 5,
                  concat = 3,
                  str = 0,
                  signed = 1,
                  unsigned = 5
                },
            mod =
              ProbMod
                { dropOutput = 0,
                  keepOutput = 1
                }
          },
      property =
        ConfProperty
          { size = 50,
            seed = Nothing,
            stmntDepth = 3,
            modDepth = 2,
            maxModules = 5,
            sampleMethod = "random",
            sampleSize = 10,
            combine = False,
            nonDeterminism = 0,
            determinism = 1,
            defaultYosys = Nothing
          }
    }

-- | Pick some random semantics-preserving transformations and apply them
randomizeSP :: Data a => a -> Gen a
randomizeSP val = do
  transformations <-
    Hog.list (Hog.linear 1 10)
      . Hog.element
      . map (anywherePossibly 0.5)
      $ exprTransformsSP
  foldrM applySPTransformation val transformations

-- | Pick some random semantics-preserving transformations and apply them
randomizeNSP :: Data (ast AnnTransform) => ast AnnTransform -> Gen (ast AnnTransform)
randomizeNSP val = do
  transformation <- Hog.element exprTransformsNSP
  applyNSPTransformation (somewhereReachable transformation) val

main :: IO ()
main = do
  setLocaleEncoding utf8
  m1 :: ModDecl () <- Hog.sample (randomMod 2 4) <&> (#id .~ Identifier "mod1")
  m2 <- Hog.sample (randomizeSP m1) <&> (#id .~ Identifier "mod2")
  m3 <- Hog.sample (randomizeNSP (annotateForTransformations m1)) <&> (#id .~ Identifier "mod2")

  equivResult <- runVCFormal (Experiment m1 m2 "vcf_fuzz_equiv")
  if equivResult.proofFound
    then putStrLn "Proof found. Equivalent modules are equivalent"
    else putStrLn "ERROR | No proof found. Equivalent modules are non-equivalent"

  putStrLn "---------------"

  nonEquivResult <- runVCFormal (Experiment m1 m3 "vcf_fuzz_nequiv")
  if nonEquivResult.proofFound
    then putStrLn "ERROR | Proof found. Non-equivalent modules are equivalent"
    else putStrLn "No proof found. Non-equivalent modules are non-equivalent"
