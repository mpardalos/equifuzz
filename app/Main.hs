module Main where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.IO.Encoding
import Hedgehog.Gen qualified as Hog
import Optics
import Shelly ((<.>), (</>))
import Shelly qualified as Sh
import Verismith.Generate (randomMod)
import Verismith.Verilog

vcfCompareScript :: Text -> Identifier -> Text -> Identifier -> Text
vcfCompareScript specfile spectop implfile impltop =
  T.unlines
    [ "set_custom_solve_script \"orch_multipliers\"",
      "set_user_assumes_lemmas_procedure \"miter\"",
      "",
      "create_design -name spec -top " <> spectop.getIdentifier <> " -lang mx",
      "vcs -sverilog " <> specfile,
      "compile_design spec",
      "",
      "create_design -name impl -top " <> impltop.getIdentifier <> " -lang mx",
      "vcs -sverilog " <> implfile,
      "compile_design impl",
      "",
      "proc miter {} {",
      "        map_by_name -inputs -implphase 1 -specphase 1",
      "        map_by_name -outputs -implphase 1 -specphase 1",
      "}",
      "",
      "compose",
      "solveNB proof",
      "proofwait",
      "listproof",
      "quit"
    ]

setUpVCFormal ::
  Show a =>
  ModDecl a ->
  ModDecl a ->
  FilePath ->
  IO ()
setUpVCFormal mod1 mod2 dir = Sh.shelly $ do
  let file1 = mod1.id.getIdentifier <> ".v"
  let file2 = mod2.id.getIdentifier <> ".v"

  Sh.mkdir dir
  Sh.writefile (dir </> ("compare.tcl" :: Text)) (vcfCompareScript file1 mod1.id file2 mod2.id)
  Sh.writefile (dir </> file1) (genSource mod1)
  Sh.writefile (dir </> file2) (genSource mod2)

boxed :: Text -> Text -> Text
boxed title =
  T.unlines
    . (["┌───" <> title <> "───"] <>)
    . (<> ["└──────"])
    . map ("│ " <>)
    . T.lines

main :: IO ()
main = do
  setLocaleEncoding utf8
  m :: ModDecl () <- Hog.sample (randomMod 2 4)
  setUpVCFormal
    (m & #id .~ Identifier "mod1")
    (m & #id .~ Identifier "mod2")
    "vcf_fuzz"
