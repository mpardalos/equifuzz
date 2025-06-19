{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Runners.SLEC (slec) where

import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments.Types (Experiment (..), ExperimentResult (..))
import Optics ((^.))
import Runners.Types (EquivalenceCheckerConfig (..))
import Runners.Util (verilogImplForEvals)
import SystemC qualified as SC

slec :: EquivalenceCheckerConfig
slec =
  EquivalenceCheckerConfig
    { name = "slec"
    , runScript = "slec compare.tcl; echo '-- slec.log --'; cat calypto/slec.log; echo 'Done'"
    , makeFiles
    , parseOutput
    }
 where
  makeFiles Experiment{design, knownEvaluations} =
    [ (specFilename, wrappedProgram)
    , (implFilename, implProgram)
    , ("compare.tcl", compareScript)
    ]
   where
    specFilename = "spec.cpp"
    implFilename = "impl.sv"

    implProgram :: Text
    implProgram = verilogImplForEvals slecTypeWidths design knownEvaluations

    wrappedProgram :: Text
    wrappedProgram =
      [__i|
            \#define SC_INCLUDE_FX
            \#include <systemc.h>

            #{SC.genSource design}

            SC_MODULE(spec) {
              sc_out<#{outType}> out;

              SC_CTOR(spec) {
                SC_METHOD(update);
              }

              void update() {
                out = #{design ^. #name}(#{inputNames});
              }
            };
            |]

    outType :: Text
    outType = SC.genSource design.returnType

    inputNames :: Text
    inputNames = T.intercalate ", " [name | (_, name) <- design.args]

    compareScript :: Text
    compareScript =
      [__i|
          build_design -spec #{specFilename}
          build_design -impl #{implFilename}
          verify
            |]

  parseOutput Experiment{experimentId} fullOutput =
    let proofSuccessful =
          ("Output-Maps                 1" `T.isPrefixOf`) `any` T.lines fullOutput
        proofFailed =
          ("Output-Maps                 0           0              0         1" `T.isPrefixOf`) `any` T.lines fullOutput
        proofFound = case (proofSuccessful, proofFailed) of
          (True, False) -> Just True
          (False, True) -> Just False
          _ -> Nothing
     in ExperimentResult{proofFound, counterExample = Nothing, fullOutput, experimentId}

slecTypeWidths :: SC.SCType -> Int
slecTypeWidths (SC.SCInt n) = n
slecTypeWidths (SC.SCUInt n) = n
slecTypeWidths (SC.SCBigInt n) = n
slecTypeWidths (SC.SCBigUInt n) = n
slecTypeWidths SC.SCFixed{w} = w
slecTypeWidths SC.SCUFixed{w} = w
slecTypeWidths SC.SCFxnumSubref{width} = width
slecTypeWidths SC.SCIntSubref{width} = width
slecTypeWidths SC.SCUIntSubref{width} = width
slecTypeWidths SC.SCSignedSubref{width} = width
slecTypeWidths SC.SCUnsignedSubref{width} = width
slecTypeWidths SC.SCIntBitref = 1
slecTypeWidths SC.SCUIntBitref = 1
slecTypeWidths SC.SCSignedBitref = 1
slecTypeWidths SC.SCUnsignedBitref = 1
slecTypeWidths SC.SCLogic = 1
slecTypeWidths SC.SCBV{width} = width
slecTypeWidths SC.SCLV{width} = width
slecTypeWidths SC.CUInt = 32
slecTypeWidths SC.CInt = 32
slecTypeWidths SC.CDouble = 64
slecTypeWidths SC.CBool = 1
