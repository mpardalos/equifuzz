{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Runners.VCF (vcFormal) where

import Data.Function ((&))
import Data.Map qualified as Map
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments
import Runners.Common (EquivalenceCheckerConfig (..))

-- | Run an experiment using VC Formal on a remote host
vcFormal :: EquivalenceCheckerConfig
vcFormal =
  EquivalenceCheckerConfig
    { name = "vcf"
    , runScript = "vcf -fmode DPV -f compare.tcl; (cat counter_example.txt || echo __NO_CEX__)"
    , makeFiles
    , parseOutput
    }
 where
  makeFiles Experiment{..} =
    [ (scFilename, systemCProgram)
    , (verilogFilename, verilogDesign)
    , ("compare.tcl", compareScript)
    ]
   where
    scFilename :: Text = "spec.cpp"
    scTopName :: Text = "spec"

    systemCProgram :: Text
    systemCProgram =
      [__i|
        \#define SC_INCLUDE_FX
        \#include <systemc>"
        #{scDesign}
        #{systemCHectorWrapper scTopName scSignature}
        |]

    verilogFilename :: Text = "impl.sv"
    verilogTopName :: Text = "top"

    compareScript :: Text
    compareScript =
      [__i|
         set_custom_solve_script "orch_multipliers"

         create_design -name spec -top #{scTopName}
         scdtan -DSC_INCLUDE_FX #{scFilename}
         compile_design spec

         create_design -name impl -top #{verilogTopName}
         vcs -sverilog #{verilogFilename}
         compile_design impl

         set_user_assumes_lemmas_procedure "miter"
         proc miter {} {
           map_by_name -inputs -specphase 1 -implphase 1
           lemma out_equiv = spec.out(1) == impl.out(1)
         }

         compose
         solveNB proof
         proofwait
         listproof
         simcex -txt counter_example.txt out_equiv
         exit
         exit
          |]

  parseOutput Experiment{..} fullOutput =
    let proofSuccessful =
          fullOutput
            & T.lines
            & any ("Status for proof \"proof\": SUCCESSFUL" `T.isInfixOf`)

        proofFailed =
          fullOutput
            & T.lines
            & any ("Status for proof \"proof\": FAILED" `T.isInfixOf`)

        proofFound = case (proofSuccessful, proofFailed) of
          (True, False) -> Just True
          (False, True) -> Just False
          _ -> Nothing

        counterExample = "TODO"
     in ExperimentResult{proofFound, counterExample = Just counterExample, fullOutput, experimentId, extraInfos = Map.empty}

-- | When doing equivalence checking with Hector (VC Formal) the code under test
-- needs to be presented to hector using a wrapper
systemCHectorWrapper :: Text -> TextSignature -> Text
systemCHectorWrapper wrapperName Signature {returnType, args, name} =
  [__i|
      \#include<Hector.h>

      void #{wrapperName}() {
          #{inputDeclarations}
          #{returnType} out;

          #{hectorRegisterInputs}
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
      [ argType <> " " <> argName <> ";"
      | (argType, argName) <- args
      ]

  hectorRegisterInputs :: Text
  hectorRegisterInputs =
    T.intercalate
      "\n    "
      [ "Hector::registerInput(\"" <> argName <> "\", " <> argName <> ");"
      | (_, argName) <- args
      ]

  argList :: Text
  argList = T.intercalate ", " [argName | (_, argName) <- args]
