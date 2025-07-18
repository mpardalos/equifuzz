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
import Experiments
import Optics ((^.))
import Runners.Types (EquivalenceCheckerConfig (..))
import SystemC qualified as SC
import qualified Data.Map as Map

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
  makeFiles Experiment{scDesign, verilogDesign} =
    [ (specFilename, wrappedProgram)
    , (implFilename, verilogDesign)
    , ("compare.tcl", compareScript)
    ]
   where
    specFilename = "spec.cpp"
    implFilename = "impl.sv"

    wrappedProgram :: Text
    wrappedProgram =
      [__i|
              \#include <systemc.h>
              \#include <jasperc.h>

              #{SC.genSource scDesign}

              int main() {
                  #{declareInputs}

                  #{registerInputs}

                  #{outType} out = #{scDesign ^. #name}(#{inputNames});

                  JASPER_OUTPUT(out);

                  return 0;
              }
              |]

    outType :: Text
    outType = SC.genSource scDesign.returnType

    declareInputs :: Text
    declareInputs =
      T.unlines
        [ SC.genSource t <> " " <> name <> ";"
        | (t, name) <- scDesign.args
        ]

    registerInputs :: Text
    registerInputs =
      T.unlines
        [ "JASPER_INPUT(" <> name <> ");"
        | (_, name) <- scDesign.args
        ]

    inputNames :: Text
    inputNames = T.intercalate ", " [name | (_, name) <- scDesign.args]

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
     in ExperimentResult{proofFound, counterExample = Nothing, fullOutput, experimentId, extraInfos = Map.empty}
