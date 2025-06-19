{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Runners.VCF (vcFormal) where

import Data.Function ((&))
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments.Types
import Runners.Types (EquivalenceCheckerConfig (..))
import SystemC qualified as SC
import Runners.Util (verilogImplForEvals)

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
    , (verilogFilename, verilogProgram)
    , ("compare.tcl", compareScript)
    ]
   where
    scFilename :: Text = "spec.cpp"
    scTopName :: Text = "spec"

    systemCProgram :: Text
    systemCProgram =
      [__i|
        #{SC.includeHeader}
        #{SC.genSource design}
        #{systemCHectorWrapper scTopName design}
        |]

    verilogFilename :: Text = "impl.sv"
    verilogTopName :: Text = "top"

    verilogProgram :: Text
    verilogProgram = verilogImplForEvals vcfTypeWidths design knownEvaluations

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

         compose -nospec
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
     in ExperimentResult{proofFound, counterExample = Just counterExample, fullOutput, experimentId}

-- | When doing equivalence checking with Hector (VC Formal) the code under test
-- needs to be presented to hector using a wrapper
systemCHectorWrapper :: Text -> SC.FunctionDeclaration -> Text
systemCHectorWrapper wrapperName SC.FunctionDeclaration{returnType, args, name} =
  [__i|
      \#include<Hector.h>

      void #{wrapperName}() {
          #{inputDeclarations}
          #{outType} out;

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
      [ SC.genSource argType <> " " <> argName <> ";"
      | (argType, argName) <- args
      ]

  outType :: Text
  outType = SC.genSource returnType

  hectorRegisterInputs :: Text
  hectorRegisterInputs =
    T.intercalate
      "\n    "
      [ "Hector::registerInput(\"" <> argName <> "\", " <> argName <> ");"
      | (_, argName) <- args
      ]

  argList :: Text
  argList = T.intercalate ", " [argName | (_, argName) <- args]

vcfTypeWidths :: SC.SCType -> Int
vcfTypeWidths (SC.SCInt n) = n
vcfTypeWidths (SC.SCUInt n) = n
vcfTypeWidths (SC.SCBigInt n) = n
vcfTypeWidths (SC.SCBigUInt n) = n
vcfTypeWidths SC.SCFixed{w} = w
vcfTypeWidths SC.SCUFixed{w} = w
vcfTypeWidths SC.SCFxnumSubref{width} = width
vcfTypeWidths SC.SCIntSubref{width} = width
vcfTypeWidths SC.SCUIntSubref{width} = width
vcfTypeWidths SC.SCSignedSubref{width} = width
vcfTypeWidths SC.SCUnsignedSubref{width} = width
vcfTypeWidths SC.SCIntBitref = 1
vcfTypeWidths SC.SCUIntBitref = 1
vcfTypeWidths SC.SCSignedBitref = 1
vcfTypeWidths SC.SCUnsignedBitref = 1
vcfTypeWidths SC.SCLogic = 1
vcfTypeWidths SC.SCBV{width} = width
vcfTypeWidths SC.SCLV{width} = width
vcfTypeWidths SC.CUInt = 32
vcfTypeWidths SC.CInt = 32
vcfTypeWidths SC.CDouble = 64
vcfTypeWidths SC.CBool = 1
