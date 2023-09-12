{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Runners.VCF (runVCFormal) where

import Control.Monad (void)
import Data.Function ((&))
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments.Types
import Optics ((^.))
import Runners.Types (SSHConnectionTarget (..))
import Shelly ((</>))
import Shelly qualified as Sh
import SystemC qualified as SC
import Util (bashExec, bashExec_)

-- | Run an experiment using VC Formal on a remote host
runVCFormal :: SSHConnectionTarget -> Maybe Text -> Experiment -> IO ExperimentResult
runVCFormal sshOpts mSourcePath experiment@Experiment {experimentId, design} = Sh.shelly . Sh.silently $ do
  let sshString = sshOpts.username <> "@" <> sshOpts.host
  let ssh :: Text = case sshOpts.password of
        Nothing -> "ssh -o PasswordAuthentication=no -o StrictHostKeychecking=no"
        Just pass -> [i|sshpass -p #{pass} ssh -o StrictHostKeychecking=no|]
  let scp :: Text = case sshOpts.password of
        Nothing -> "scp -o PasswordAuthentication=no -o StrictHostKeychecking=no"
        Just pass -> [i|sshpass -p #{pass} scp -o StrictHostKeychecking=no|]

  localExperimentDir <- T.strip <$> Sh.run "mktemp" ["-d"]

  Sh.writefile
    (localExperimentDir </> filename)
    wrappedProgram
  Sh.writefile
    (localExperimentDir </> ("compare.tcl" :: Text))
    compareScript
  bashExec_
    [i|#{ssh} #{sshString} mkdir -p #{remoteExperimentDir}/ |]
  bashExec_
    [i|#{scp} -r #{localExperimentDir}/* #{sshString}:#{remoteExperimentDir}/|]

  fullOutput <- bashExec [i|#{ssh} #{sshString} '#{sshCommand}'|]

  void . Sh.errExit False $
    bashExec [i|#{scp} #{sshString}:#{remoteExperimentDir}/counter_example.txt #{localExperimentDir}/counter_example.txt|]

  bashExec_ [i|#{ssh} #{sshString} 'cd ~ && rm -rf ./#{remoteExperimentDir}'|]

  counterExample <-
    Sh.test_f (T.unpack localExperimentDir <> "/counter_example.txt") >>= \case
      False -> pure Nothing
      True -> Just <$> Sh.readfile (T.unpack localExperimentDir <> "/counter_example.txt")

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

  return $ ExperimentResult {proofFound, counterExample, fullOutput, experimentId}
  where
    remoteDir :: Text
    remoteDir = "equifuzz_vcf_experiment"

    remoteExperimentDir :: Text
    remoteExperimentDir = [i|#{remoteDir}/#{experimentId ^. #uuid}|]

    filename :: Text = "impl.cpp"

    topName :: Text = "impl"

    sshCommand :: Text
    sshCommand = case mSourcePath of
      Just sourcePath -> [i|cd #{remoteExperimentDir} && ls -ltr && md5sum * && source #{sourcePath} && vcf -fmode DPV -f compare.tcl|]
      Nothing -> [i|cd #{remoteExperimentDir} && ls -ltr && md5sum * && vcf -fmode DPV -f compare.tcl|]

    wrappedProgram :: Text
    wrappedProgram =
      [__i|
          #{SC.includeHeader}

          #{SC.genSource design}

          #{systemCHectorWrapper topName design}
          |]

    compareScript :: Text
    compareScript =
      [__i|
                set_custom_solve_script "orch_multipliers"
                set_user_assumes_lemmas_procedure "miter"

                create_design -name impl -top #{topName}
                scdtan -DSC_INCLUDE_FX #{filename}
                compile_design impl

                proc miter {} {
                        lemma out_equiv = out(1) == #{experiment ^. #comparisonValue}
                }

                compose -nospec
                solveNB proof
                proofwait
                listproof
                simcex -txt counter_example.txt out_equiv
                exit
                exit
      |]

-- | When doing equivalence checking with Hector (VC Formal) the code under test
-- needs to be presented to hector using a wrapper
systemCHectorWrapper :: SC.Annotation ann => Text -> SC.FunctionDeclaration ann -> Text
systemCHectorWrapper wrapperName SC.FunctionDeclaration {returnType, args, name} =
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
