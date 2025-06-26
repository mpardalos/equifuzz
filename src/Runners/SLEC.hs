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
            \#define SC_INCLUDE_FX
            \#include <systemc.h>

            #{SC.genSource scDesign}

            SC_MODULE(spec) {
              sc_out<#{outType}> out;

              SC_CTOR(spec) {
                SC_METHOD(update);
              }

              void update() {
                out = #{scDesign ^. #name}(#{inputNames});
              }
            };
            |]

    outType :: Text
    outType = SC.genSource scDesign.returnType

    inputNames :: Text
    inputNames = T.intercalate ", " [name | (_, name) <- scDesign.args]

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
