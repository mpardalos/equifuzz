{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Runners.SLEC (runSLEC) where

import Control.Monad (void)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments.Types (Experiment (..), ExperimentResult (..))
import Optics ((^.))
import Runners.Types (SSHConnectionTarget)
import Runners.Util (createRemoteExperimentDir, runSSHCommand)
import Shelly qualified as Sh
import SystemC qualified as SC

runSLEC :: SSHConnectionTarget -> Maybe Text -> Experiment -> IO ExperimentResult
runSLEC sshOpts mSourcePath Experiment{experimentId, design, comparisonValue} = Sh.shelly . Sh.silently $ do
  createRemoteExperimentDir
    sshOpts
    remoteExperimentDir
    [ (specFilename, wrappedProgram)
    , (implFilename, implProgram)
    , ("compare.tcl", compareScript)
    ]

  fullOutput <- runSSHCommand sshOpts sshCommand

  void $ runSSHCommand sshOpts [i|cd ~ && rm -rf ./#{remoteExperimentDir}|]

  let proofSuccessful =
        ("Output-Maps                 1" `T.isPrefixOf`) `any` T.lines fullOutput
  let proofFailed =
        ("Output-Maps                 0           0              0         1" `T.isPrefixOf`) `any` T.lines fullOutput

  let proofFound = case (proofSuccessful, proofFailed) of
        (True, False) -> Just True
        (False, True) -> Just False
        _ -> Nothing

  return $ ExperimentResult{proofFound, counterExample = Nothing, fullOutput, experimentId}
 where
  remoteDir :: Text
  remoteDir = "equifuzz_slec_experiment"

  remoteExperimentDir :: Text
  remoteExperimentDir = [i|#{remoteDir}/#{experimentId ^. #uuid}|]

  specFilename :: Text = "spec.cpp"
  implFilename :: Text = "impl.sv"

  sourceCommand :: Text
  sourceCommand = case mSourcePath of
    Just sourcePath -> [i|source #{sourcePath}|]
    Nothing -> [i|echo 'Not sourcing anything'|]

  sshCommand :: Text
  sshCommand =
    [i|cd #{remoteExperimentDir} && pwd && ls -ltr && #{sourceCommand} && slec compare.tcl; echo '-- slec.log --'; cat calypto/slec.log; echo 'Done' |]

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
