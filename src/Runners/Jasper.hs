{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Runners.Jasper (jasper) where

import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments.Types (Experiment (..), ExperimentResult (..))
import Optics ((^.))
import Runners.Types (EquivalenceCheckerConfig (..))
import Runners.Util (verilogImplForEvals)
import SystemC qualified as SC

default (T.Text)

jasper :: EquivalenceCheckerConfig
jasper =
  EquivalenceCheckerConfig
    { name = "jasper"
    , runScript = "jg -c2rtl -allow_unsupported_OS -batch -tcl compare.tcl"
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
    implProgram = verilogImplForEvals jasperTypeWidths design knownEvaluations

    wrappedProgram :: Text
    wrappedProgram =
      [__i|
              \#include <systemc.h>
              \#include <jasperc.h>

              #{SC.genSource design}

              int main() {
                  #{declareInputs}

                  #{registerInputs}

                  #{outType} out = #{design ^. #name}(#{inputNames});

                  JASPER_OUTPUT(out);

                  return 0;
              }
              |]

    outType :: Text
    outType = SC.genSource design.returnType

    declareInputs :: Text
    declareInputs =
      T.unlines
        [ SC.genSource t <> " " <> name <> ";"
        | (t, name) <- design.args
        ]

    registerInputs :: Text
    registerInputs =
      T.unlines
        [ "JASPER_INPUT(" <> name <> ");"
        | (_, name) <- design.args
        ]

    inputNames :: Text
    inputNames = T.intercalate ", " [name | (_, name) <- design.args]

    compareScript :: Text
    compareScript =
      [__i|
            check_c2rtl -set_dynamic_pruning -spec; check_c2rtl -compile -spec #{specFilename}
            check_c2rtl -analyze -imp -sv #{implFilename} ;
            check_c2rtl -elaborate -imp -top top
            check_c2rtl -setup
            clock -clear; clock -none
            reset -none;
            check_c2rtl -generate_verification
            check_c2rtl -interface -task <embedded>
            prove -all
              |]

  parseOutput Experiment{experimentId} fullOutput =
    let proofSuccessful =
          ("- proven                    : 1" `T.isInfixOf`) `any` T.lines fullOutput
        proofFailed =
          ("- cex                       : 1" `T.isInfixOf`) `any` T.lines fullOutput

        proofFound = case (proofSuccessful, proofFailed) of
          (True, False) -> Just True
          (False, True) -> Just False
          _ -> Nothing
     in ExperimentResult{proofFound, counterExample = Nothing, fullOutput, experimentId}

jasperTypeWidths :: SC.SCType -> Int
jasperTypeWidths (SC.SCInt n) = n
jasperTypeWidths (SC.SCUInt n) = n
jasperTypeWidths (SC.SCBigInt n) = n
jasperTypeWidths (SC.SCBigUInt n) = n
jasperTypeWidths SC.SCFixed{w} = w
jasperTypeWidths SC.SCUFixed{w} = w
jasperTypeWidths SC.SCFxnumSubref{width} = width
jasperTypeWidths SC.SCIntSubref{width} = width
jasperTypeWidths SC.SCUIntSubref{width} = width
jasperTypeWidths SC.SCSignedSubref{width} = width
jasperTypeWidths SC.SCUnsignedSubref{width} = width
jasperTypeWidths SC.SCIntBitref = 1
jasperTypeWidths SC.SCUIntBitref = 1
jasperTypeWidths SC.SCSignedBitref = 1
jasperTypeWidths SC.SCUnsignedBitref = 1
jasperTypeWidths SC.SCLogic = 1
jasperTypeWidths SC.SCBV{width} = width
jasperTypeWidths SC.SCLV{width} = width
jasperTypeWidths SC.CUInt = 32
jasperTypeWidths SC.CInt = 32
jasperTypeWidths SC.CDouble = 64
jasperTypeWidths SC.CBool = 1
