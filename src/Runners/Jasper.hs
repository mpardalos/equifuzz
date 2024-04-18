{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Runners.Jasper (runJasper) where

import Control.Monad (void)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments.Types (Experiment (..), ExperimentResult (..))
import Optics ((^.))
import Runners.Types (SSHConnectionTarget)
import Runners.Util (createLocalExperimentDir, createRemoteExperimentDir, runSSHCommand)
import Shelly qualified as Sh
import SystemC qualified as SC
import Util (bashExec, bashExec_)

runJasper :: SSHConnectionTarget -> Maybe Text -> Experiment -> IO ExperimentResult
runJasper sshOpts mSourcePath Experiment {experimentId, design, comparisonValue} = Sh.shelly . Sh.silently $ do
  createLocalExperimentDir
    remoteExperimentDir
    [ (specFilename, wrappedProgram),
      (implFilename, implProgram),
      ("compare.tcl", compareScript)
    ]

  fullOutput <- bashExec sshCommand

  bashExec_ [i|rm -rf ./#{remoteExperimentDir}|]

  let proofSuccessful =
        ("- proven                    : 1" `T.isInfixOf`) `any` T.lines fullOutput
  let proofFailed =
        ("- cex                       : 1" `T.isInfixOf`) `any` T.lines fullOutput

  let proofFound = case (proofSuccessful, proofFailed) of
        (True, False) -> Just True
        (False, True) -> Just False
        _ -> Nothing

  return $ ExperimentResult {proofFound, counterExample = Nothing, fullOutput, experimentId}
  where
    remoteDir :: Text
    remoteDir = "equifuzz_jasper_experiment"

    remoteExperimentDir :: Text
    remoteExperimentDir = [i|#{remoteDir}/#{experimentId ^. #uuid}|]

    specFilename :: Text = "spec.cpp"
    implFilename :: Text = "impl.sv"

    sshCommand :: Text
    sshCommand = case mSourcePath of
      Just sourcePath -> [i|cd #{remoteExperimentDir} && ls -ltr && source #{sourcePath} && jg -c2rtl -allow_unsupported_OS -batch -tcl compare.tcl; echo 'Done'|]
      Nothing -> [i|cd #{remoteExperimentDir} && ls -ltr && jg -c2rtl -allow_unsupported_OS -batch -tcl compare.tcl; echo 'Done'|]

    implProgram :: Text
    implProgram =
      [__i|
          module top (output [#{comparisonValue ^. #width - 1}:0] out);
                assign out = #{comparisonValue ^. #literal};
          endmodule
          |]

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
        [ "JASPER_INPUT("<> name <> ");"
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
        prove -all -bg
          |]
