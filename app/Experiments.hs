{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments where

import GenSystemC (genSystemCConstant)
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Data.Data (Data)
import Data.Maybe (fromJust)
import Data.String (IsString)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Hedgehog.Gen qualified as Hog
import Optics (makeFieldLabelsNoPrefix, (&), (<&>), (^.))
import Safe (fromJustNote, tailDef)
import Shelly ((<.>), (</>))
import Shelly qualified as Sh
import System.FilePath (takeBaseName, takeExtension)
import SystemC qualified as SC
import Text.Printf (printf)

data Experiment = Experiment
  { uuid :: UUID,
    -- | True if we expect the modules to be equivalent, False if we expect them not to be
    expectedResult :: Bool,
    design1 :: DesignSource,
    design2 :: DesignSource
  }

instance Show Experiment where
  show e =
    printf
      "Experiment { uuid=\"%s\", expectedResult = \"%s\", design1 = ..., design2 = ... }"
      (show e.uuid)
      (show e.expectedResult)

instance Eq Experiment where
  e1 == e2 = e1.uuid == e2.uuid

instance Ord Experiment where
  compare e1 e2 = compare e1.uuid e2.uuid

data DesignLanguage = SystemC | Verilog

data DesignSource = DesignSource
  { language :: DesignLanguage,
    topName :: Text,
    source :: Text
  }

data ExperimentResult = ExperimentResult
  { proofFound :: Maybe Bool,
    counterExample :: Maybe Text,
    fullOutput :: Text,
    uuid :: UUID
  }
  deriving (Show, Eq, Generic, Data)

makeFieldLabelsNoPrefix ''Experiment
makeFieldLabelsNoPrefix ''DesignSource
makeFieldLabelsNoPrefix ''ExperimentResult

data ExperimentProgress
  = Began Experiment
  | Aborted Experiment
  | Completed ExperimentResult
  deriving (Show)

type ProgressNotify = ExperimentProgress -> IO ()

-- | Run an experiment using VC Formal on a fixed remote host (ee-mill3)
runVCFormal :: Experiment -> IO ExperimentResult
runVCFormal Experiment {design1, design2, uuid} = Sh.shelly . Sh.silently $ do
  dir <- T.strip <$> Sh.run "mktemp" ["-d"]
  Sh.writefile (dir </> ("compare.tcl" :: Text)) compareScript
  Sh.writefile (dir </> design1Filename) design1.source
  Sh.writefile (dir </> design2Filename) design2.source
  void $ Sh.bash "ssh" [vcfHost, "mkdir -p " <> remoteDir <> "/"]
  void $ Sh.bash "scp" ["-r", dir <> "/*", vcfHost <> ":" <> remoteDir <> "/" <> T.pack (show uuid)]

  fullOutput <- Sh.silently $ Sh.run "ssh" [vcfHost, sshCommand]

  void . Sh.errExit False $
    Sh.bash "scp" [vcfHost <> ":" <> remoteDir <> "/" <> T.pack (show uuid) <> "/counter_example.txt", dir <> "/counter_example.txt"]

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

  return ExperimentResult {proofFound, counterExample, fullOutput, uuid}
  where
    remoteDir :: Text
    remoteDir = "equifuzz_vcf_experiment"

    design1Filename :: Text
    design1Filename = "design1." <> languageFileExtension design1.language

    design2Filename :: Text
    design2Filename = "design2." <> languageFileExtension design2.language

    experimentDir :: Text
    experimentDir = remoteDir <> "/" <> T.pack (show uuid)

    sshCommand :: Text
    sshCommand = [i|cd #{experimentDir} && ls -ltr && md5sum *.v && vcf -fmode DPV -f compare.tcl|]

    compileCommand :: DesignLanguage -> Text -> Text
    compileCommand language file = case language of
      Verilog -> [i|vcs -sverilog #{file}|]
      SystemC -> [i|cppan #{file}|]

    compareScript :: Text
    compareScript =
      -- TODO: Add input assumptions by hand instead of map_by_name (use DesignSource.inputNames)
      [__i|
                set_custom_solve_script "orch_multipliers"
                set_user_assumes_lemmas_procedure "miter"

                create_design -name spec -top #{design1 ^. #topName}
                #{compileCommand (design1 ^. #language) design1Filename}
                compile_design spec

                create_design -name impl -top #{design2 ^. #topName}
                #{compileCommand (design2 ^. #language) design2Filename}
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

    vcfHost :: Text
    vcfHost = "mp5617@ee-mill3.ee.ic.ac.uk"

-- | Save information about the experiment to the experiments/ directory
saveExperiment :: String -> Experiment -> ExperimentResult -> IO ()
saveExperiment category Experiment {design1, design2, uuid, expectedResult} ExperimentResult {fullOutput, proofFound} = Sh.shelly . Sh.silently $ do
  let dir = "experiments/" <> category <> "/" <> UUID.toString uuid
  let info :: Text =
        [__i|
            Expected equivalence: #{show expectedResult}
            Found equivalence:    #{show proofFound}
            |]

  Sh.mkdir_p dir
  Sh.writefile (dir </> ("design1" :: Text) <.> languageFileExtension design1.language) design1.source
  Sh.writefile (dir </> ("design2" :: Text) <.> languageFileExtension design2.language) design2.source
  Sh.writefile (dir </> ("full_output.txt" :: Text)) fullOutput
  Sh.writefile (dir </> ("info.txt" :: Text)) info

-- | Make an experiment using the SystemC-constant generator. Needs to have
-- icarus verilog (`iverilog`) available locally
mkSystemCConstantExperiment :: IO Experiment
mkSystemCConstantExperiment = do
  systemcModule <- Hog.sample (genSystemCConstant "mod1")
  let design1 =
        DesignSource
          { language = SystemC,
            topName = hectorWrapperName,
            source =
              SC.includeHeader
                <> "\n\n"
                <> SC.genSource systemcModule
                <> "\n\n"
                <> systemCHectorWrapper systemcModule
          }

  expectedResult <-
    simulateSystemCConstant systemcModule
      <&> T.drop 2 -- 0b prefix
      <&> T.replace "." "" -- Remove decimal point if present

  -- FIXME: The error from this `fromJust` is unhandled up the stack. Handle
  -- errors from experiment generation
  let outWidth = fromJust $ SC.specifiedWidth systemcModule.returnType
  let signed :: Text = if SC.isSigned systemcModule.returnType then "signed" else ""
  let verilogModule =
        [__i|
          module mod2 (out);
            output wire #{signed} [#{outWidth - 1}:0] out;
            assign out = #{outWidth}'b#{expectedResult};
          endmodule
            |]
  let design2 =
        DesignSource
          { language = Verilog,
            topName = "mod2",
            source = verilogModule
          }
  uuid <- UUID.nextRandom
  return Experiment {expectedResult = True, ..}

-- | When doing equivalence checking with Hector (VC Formal) the code under test
-- needs to be presented to hector using a wrapper
systemCHectorWrapper :: SC.Annotation ann => SC.FunctionDeclaration ann -> Text
systemCHectorWrapper SC.FunctionDeclaration {returnType, args, name} =
  [__i|
      \#include<Hector.h>

      void #{hectorWrapperName}() {
          #{inputDeclarations}
          #{outType} out;

          #{inputsHectorRegister}
          Hector::registerOutput("out", out);

          Hector::beginCapture();
          out = #{name}(#{argList});
          Hector::endCapture();
      }

      |]
  where
    inputDeclarations :: Text
    inputDeclarations =
      T.intercalate
        "\n    "
        [ SC.genSource argType <> " " <> argName <> ";"
          | (argType, argName) <- args
        ]

    outType :: Text
    outType = SC.genSource returnType

    inputsHectorRegister :: Text
    inputsHectorRegister =
      T.intercalate
        "\n    "
        [ "Hector::registerInput(\"" <> argName <> "\", " <> argName <> ");"
          | (_, argName) <- args
        ]

    argList :: Text
    argList = T.intercalate ", " [argName | (_, argName) <- args]

-- | Run the SystemC function and return its output represented as text of a binary number
-- We use this output format because the normal (decimal) output makes the
-- representation of the output non-obvious. E.g. -1 as an sc_uint<8> or a
-- sc_fixed<10,3> are represented completely differently. With the default
-- SystemC output, we would get "-1" in both cases, but here we (correctly) get
-- "0b11111111" and "0b111.0000000"
simulateSystemCConstant :: SC.Annotation ann => SC.FunctionDeclaration ann -> IO Text
simulateSystemCConstant decl@SC.FunctionDeclaration {name} = Sh.shelly . Sh.silently $ do
  let fullSource =
        [__i|
            #{SC.includeHeader}

            #{SC.genSource decl}

            int sc_main(int argc, char **argv) {
                std::cout << #{name}().to_string(sc_dt::SC_BIN) << std::endl;
                return 0;
            }
            |]

  tmpDir <- T.strip <$> Sh.run "mktemp" ["-d"]
  let cppPath = tmpDir <> "/main.cpp"
  let binPath = tmpDir <> "/main"

  Sh.writefile (T.unpack cppPath) fullSource
  _compileOutput <- Sh.bash "g++" ["-std=c++11", "-I/usr/include/systemc", "-lsystemc", cppPath, "-o", binPath]
  programOut <- T.strip <$> Sh.bash (T.unpack binPath) []
  void $ Sh.bash "rm" ["-r", tmpDir]
  return programOut

experimentLoop :: IO Experiment -> (Experiment -> IO ExperimentResult) -> ProgressNotify -> IO ()
experimentLoop generator runner progress = forever $ do
  -- FIXME: Handle errors from the generator
  experiment <- generator

  progress (Began experiment)
  runResult <- Control.Exception.try @Control.Exception.SomeException $ runner experiment
  case runResult of
    Left _ -> progress (Aborted experiment)
    Right result -> do
      case (experiment.expectedResult, result.proofFound) of
        (_, Nothing) -> saveExperiment "weird" experiment result
        (True, Just False) -> saveExperiment "false-negatives" experiment result
        (False, Just True) -> saveExperiment "false-positives" experiment result
        _ -> pure ()

      progress (Completed result)

-- | Read a design from a file into a DesignSource. Guesses the language from
-- the file extension. It raises an error if it cannot recognise the language.
designSourceFromFile :: FilePath -> IO DesignSource
designSourceFromFile path = do
  let extension = tailDef "" (takeExtension path)
  let language =
        fromJustNote
          ("Unrecognised file extension: '" ++ extension ++ "'")
          (languageFromExtension extension)
  let topName = T.pack (takeBaseName path)
  source <- T.pack <$> readFile path
  pure DesignSource {..}

languageFromExtension :: (Eq a, IsString a) => a -> Maybe DesignLanguage
languageFromExtension "c" = Just SystemC
languageFromExtension "cpp" = Just SystemC
languageFromExtension "v" = Just Verilog
languageFromExtension _ = Nothing

isCompleted :: ExperimentProgress -> Bool
isCompleted Completed {} = True
isCompleted _ = False

hectorWrapperName :: Text
hectorWrapperName = "hector_wrapper"

languageFileExtension :: DesignLanguage -> Text
languageFileExtension Verilog = "v"
languageFileExtension SystemC = "cpp"
