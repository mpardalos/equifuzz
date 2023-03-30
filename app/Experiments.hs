{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments where

import BuildOut (buildOutSystemCVerilog, buildOutVerilogVerilog)
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Data.Data (Data)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Hedgehog.Gen qualified as Hog
import Optics (ix, makeFieldLabelsNoPrefix, traversed, (%), (&), (^.), (^..), _2)
import Shelly ((</>))
import Shelly qualified as Sh
import SystemC qualified as SC
import Text.Printf (printf)
import Verismith.Verilog.AST (Identifier (), ModDecl (inPorts))
import Verismith.Verilog.CodeGen (Source (genSource))

data Experiment = Experiment
  { uuid :: UUID,
    -- | True if we expect the modules to be equivalent, False if we expect them not to be
    expectedResult :: Bool,
    design1 :: DesignSource,
    design2 :: DesignSource
  }

data DesignLanguage = SystemC | Verilog

data DesignSource = DesignSource
  { language :: DesignLanguage,
    inputNames :: [Text],
    topName :: Text,
    source :: Text
  }

makeFieldLabelsNoPrefix ''Experiment
makeFieldLabelsNoPrefix ''DesignSource

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

data ExperimentResult = ExperimentResult
  { proofFound :: Maybe Bool,
    fullOutput :: Text,
    uuid :: UUID
  }
  deriving (Show, Eq, Generic, Data)

makeFieldLabelsNoPrefix ''ExperimentResult

data ExperimentProgress
  = Began Experiment
  | Aborted Experiment
  | Completed ExperimentResult
  deriving (Show)

type ProgressNotify = ExperimentProgress -> IO ()

isCompleted :: ExperimentProgress -> Bool
isCompleted Completed {} = True
isCompleted _ = False

hectorWrapperName :: Text
hectorWrapperName = "hector_wrapper"

runVCFormal :: Experiment -> IO ExperimentResult
runVCFormal Experiment {design1, design2, uuid} = Sh.shelly . Sh.silently $ do
  dir <- T.strip <$> Sh.run "mktemp" ["-d"]
  Sh.writefile (dir </> ("compare.tcl" :: Text)) (compareScript "design1.v" design1 "design2.v" design2)
  Sh.writefile (dir </> ("design1.v" :: Text)) design1.source
  Sh.writefile (dir </> ("design2.v" :: Text)) design2.source
  void $ Sh.bash "ssh" [vcfHost, "mkdir -p " <> remoteDir <> "/"]
  void $ Sh.bash "scp" ["-r", dir <> "/*", vcfHost <> ":" <> remoteDir <> "/" <> T.pack (show uuid)]

  fullOutput <- Sh.silently $ Sh.run "ssh" [vcfHost, sshCommand]

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

  return ExperimentResult {proofFound, fullOutput, uuid}
  where
    remoteDir :: Text
    remoteDir = "equifuzz_vcf_experiment"

    experimentDir :: Text
    experimentDir = remoteDir <> "/" <> T.pack (show uuid)

    sshCommand :: Text
    sshCommand = [i|cd #{experimentDir} && ls -ltr && md5sum *.v && vcf -fmode DPV -f compare.tcl && cd ~ && rm -rf ./#{experimentDir}|]

    compileCommand :: DesignLanguage -> Text -> Text
    compileCommand language file = case language of
      Verilog -> [i|vcs -sverilog #{file}|]
      SystemC -> [i|cppan #{file}|]

    designTopName :: DesignSource -> Text
    designTopName design = case design.language of
      Verilog -> design.topName
      SystemC -> hectorWrapperName

    compareScript :: Text -> DesignSource -> Text -> DesignSource -> Text
    compareScript file1 design1 file2 design2 =
      -- TODO: Add input assumptions by hand instead of map_by_name (use DesignSource.inputNames)
      [__i|
                set_custom_solve_script "orch_multipliers"
                set_user_assumes_lemmas_procedure "miter"

                create_design -name spec -top #{designTopName design1}
                #{compileCommand (design1 ^. #language) file1}
                compile_design spec

                create_design -name impl -top #{designTopName design2}
                #{compileCommand (design2 ^. #language) file2}
                compile_design impl

                proc miter {} {
                        map_by_name -inputs -implphase 1 -specphase 1
                        lemma spec.out(1) == impl.out(1)
                }

                compose
                solveNB proof
                proofwait
                listproof
                quit
    |]

    vcfHost :: Text
    vcfHost = "mp5617@ee-mill3.ee.ic.ac.uk"

saveExperiment :: String -> Experiment -> ExperimentResult -> IO ()
saveExperiment category Experiment {design1, design2, uuid, expectedResult} ExperimentResult {fullOutput, proofFound} = Sh.shelly . Sh.silently $ do
  let dir = "experiments/" <> category <> "/" <> UUID.toString uuid
  let info :: Text =
        [__i|
            Expected equivalence: #{show expectedResult}
            Found equivalence:    #{show proofFound}
            |]

  Sh.mkdir_p dir
  Sh.writefile (dir </> ("design1.v" :: Text)) design1.source
  Sh.writefile (dir </> ("design2.v" :: Text)) design2.source
  Sh.writefile (dir </> ("full_output.txt" :: Text)) fullOutput
  Sh.writefile (dir </> ("info.txt" :: Text)) info

mkVerilogVerilogExperiment :: IO Experiment
mkVerilogVerilogExperiment = do
  (mod1, mod2) <- Hog.sample (buildOutVerilogVerilog "mod1" "mod2")
  let design1 =
        DesignSource
          { language = Verilog,
            inputNames = mod1 ^.. #inPorts % traversed % #name % #_Identifier,
            topName = "mod1",
            source = genSource mod1
          }
  let design2 =
        DesignSource
          { language = Verilog,
            inputNames = mod2 ^.. #inPorts % traversed % #name % #_Identifier,
            topName = "mod2",
            source = genSource mod2
          }
  uuid <- UUID.nextRandom
  return Experiment {expectedResult = False, ..}

mkSystemCVerilogExperiment :: IO Experiment
mkSystemCVerilogExperiment = do
  (systemcModule, verilogModule) <- Hog.sample (buildOutSystemCVerilog "mod1" "mod2")
  let design1 =
        DesignSource
          { language = SystemC,
            inputNames = systemcModule ^.. #args % traversed % _2,
            topName = "mod1",
            source =
              SC.includeHeader
                <> "\n\n"
                <> genSource systemcModule
                <> "\n\n"
                <> systemCHectorWrapper systemcModule
          }
  let design2 =
        DesignSource
          { language = Verilog,
            inputNames = verilogModule ^.. #inPorts % traversed % #name % #_Identifier,
            topName = "mod2",
            source = genSource verilogModule
          }
  uuid <- UUID.nextRandom
  return Experiment {expectedResult = False, ..}

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
        [ genSource argType <> " " <> argName <> ";"
          | (argType, argName) <- args
        ]

    outType :: Text
    outType = genSource returnType

    inputsHectorRegister :: Text
    inputsHectorRegister =
      T.intercalate
        "\n    "
        [ "Hector::registerInput(\"" <> argName <> "\", " <> argName <> ");"
          | (_, argName) <- args
        ]

    argList :: Text
    argList = T.intercalate ", " [argName | (_, argName) <- args]

experimentLoop :: IO Experiment -> (Experiment -> IO ExperimentResult) -> ProgressNotify -> IO ()
experimentLoop generator runner progress = forever $ do
  experiment <- generator

  progress (Began experiment)
  runResult <- try @SomeException $ runner experiment
  case runResult of
    Left _ -> progress (Aborted experiment)
    Right result -> do
      case (experiment.expectedResult, result.proofFound) of
        (_, Nothing) -> saveExperiment "weird" experiment result
        (True, Just False) -> saveExperiment "false-negatives" experiment result
        (False, Just True) -> saveExperiment "false-positives" experiment result
        _ -> pure ()

      progress (Completed result)
