{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, void)
import Data.Data (Data)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Hedgehog.Gen qualified as Hog
import Optics (makeFieldLabelsNoPrefix, (&), (.~), (<&>))
import Shelly ((</>))
import Shelly qualified as Sh
import Text.Printf (printf)
import Transform
  ( annotateForTransformations,
    randomizeNSP,
  )
import Verismith.Generate as Generate
  ( ConfProperty (..),
    Config (..),
    ProbExpr (..),
    ProbMod (..),
    ProbModItem (..),
    ProbStatement (..),
    Probability (..),
    proceduralSrcIO,
  )
import Verismith.Verilog (SourceInfo (..), genSource)
import Verismith.Verilog.AST (Annotation (..), Identifier (..), topModuleId)

data Experiment = forall ann1 ann2.
  (Show (AnnModDecl ann1), Show (AnnModDecl ann2)) =>
  Experiment
  { uuid :: UUID,
    -- | True if we expect the modules to be equivalent, False if we expect them not to be
    expectedResult :: Bool,
    design1 :: SourceInfo ann1,
    design2 :: SourceInfo ann2
  }

makeFieldLabelsNoPrefix ''Experiment

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
  { proofFound :: Bool,
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

runVCFormal :: Experiment -> IO ExperimentResult
runVCFormal Experiment {design1, design2, uuid} = Sh.shelly . Sh.silently $ do
  dir <- T.strip <$> Sh.run "mktemp" ["-d"]
  Sh.writefile (dir </> ("compare.tcl" :: Text)) (compareScript "design1.v" design1.top "design2.v" design2.top)
  Sh.writefile (dir </> ("design1.v" :: Text)) (genSource design1)
  Sh.writefile (dir </> ("design2.v" :: Text)) (genSource design2)
  void $ Sh.bash "ssh" [vcfHost, "mkdir -p " <> remoteDir <> "/"]
  void $ Sh.bash "scp" ["-r", dir <> "/*", vcfHost <> ":" <> remoteDir <> "/" <> T.pack (show uuid)]

  fullOutput <- Sh.silently $ Sh.run "ssh" [vcfHost, sshCommand]
  let proofFound =
        fullOutput
          & T.lines
          & any ("Status for proof \"proof\": SUCCESSFUL" `T.isInfixOf`)

  return ExperimentResult {proofFound, fullOutput, uuid}
  where
    remoteDir :: Text
    remoteDir = "equifuzz_vcf_experiment"

    sshCommand :: Text
    sshCommand = [i|cd #{remoteDir<>"/"<>T.pack (show uuid)} && ( pwd > ~/out ) && ls -ltr && md5sum *.v && vcf -fmode DPV -f compare.tcl|]

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

saveExperiment :: String -> Experiment -> ExperimentResult -> IO ()
saveExperiment category Experiment {design1, design2, uuid, expectedResult} ExperimentResult {fullOutput, proofFound} = Sh.shelly . Sh.silently $ do
  let dir = "experiments/" <> category <> "/" <> UUID.toString uuid
  let info :: Text =
        [__i|
            Expected equivalence: #{show expectedResult}
            Found equivalence:    #{show proofFound}
            |]

  Sh.mkdir_p dir
  Sh.writefile (dir </> ("design1.v" :: Text)) (genSource design1)
  Sh.writefile (dir </> ("design2.v" :: Text)) (genSource design2)
  Sh.writefile (dir </> ("full_output.txt" :: Text)) fullOutput
  Sh.writefile (dir </> ("info.txt" :: Text)) info

genConfig :: Config
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

mkNegativeExperiment :: IO Experiment
mkNegativeExperiment = do
  design1 <- proceduralSrcIO "mod1" genConfig
  design2 <- Hog.sample (randomizeNSP (annotateForTransformations design1)) <&> (topModuleId .~ Identifier "mod2")
  uuid <- UUID.nextRandom
  return Experiment {expectedResult = False, ..}

experimentLoop :: IO Experiment -> (Experiment -> IO ExperimentResult) -> ProgressNotify -> IO ()
experimentLoop generator runner progress = forever $ do
  experiment <- generator

  progress (Began experiment)
  runResult <- try @SomeException $ runner experiment
  case runResult of
    Left _ -> progress (Aborted experiment)
    Right result -> do
      case (experiment.expectedResult, result.proofFound) of
        (True, False) -> saveExperiment "false-negatives" experiment result
        (False, True) -> saveExperiment "false-positives" experiment result
        _ -> pure ()

      progress (Completed result)
