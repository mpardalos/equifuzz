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

module Runners.Util where

import Control.Monad (forM_, void)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments.Types (
  Evaluation (..),
  comparisonValueAsVerilog,
 )
import Optics
import Runners.Types (SSHConnectionTarget (..))
import SystemC qualified as SC
import Util (runBash, mkdir_p)
import qualified Data.Text.IO as TIO
import Shelly ((</>))

default (T.Text)

createExperimentDir :: Text -> [(Text, Text)] -> IO ()
createExperimentDir experimentDir files = do
  mkdir_p experimentDir

  forM_ files $ \(filename, content) ->
    TIO.writeFile (experimentDir </> filename) content

createRemoteExperimentDir :: SSHConnectionTarget -> Text -> [(Text, Text)] -> IO ()
createRemoteExperimentDir sshOpts remoteExperimentDir files = do
  localExperimentDir <- T.strip <$> runBash "mktemp -d"

  forM_ files $ \(filename, content) ->
    TIO.writeFile (localExperimentDir </> filename) content

  void $ runSSHCommand sshOpts [i|mkdir -p #{remoteExperimentDir}/|]
  void $ scpUpload sshOpts [i|#{localExperimentDir}/*|] [i|#{remoteExperimentDir}/|]

runSSHCommand :: SSHConnectionTarget -> Text -> IO Text
runSSHCommand sshOpts commandStr = runBash [i|#{ssh} #{sshString} '#{commandStr}'|]
 where
  sshString = sshOpts.username <> "@" <> sshOpts.host
  ssh :: Text = case sshOpts.password of
    Nothing -> "ssh -o PasswordAuthentication=no -o StrictHostKeychecking=no"
    Just pass -> [i|sshpass -p #{pass} ssh -o StrictHostKeychecking=no|]

scpDownload ::
  SSHConnectionTarget ->
  -- | Remote path
  Text ->
  -- | Local path
  Text ->
  IO Text
scpDownload sshOpts remote local =
  runBash [i|#{scp} #{sshString}:#{remote} #{local}|]
 where
  sshString = sshOpts.username <> "@" <> sshOpts.host
  scp :: Text = case sshOpts.password of
    Nothing -> "scp -o PasswordAuthentication=no -o StrictHostKeychecking=no"
    Just pass -> [i|sshpass -p #{pass} scp -o StrictHostKeychecking=no|]

scpUpload ::
  SSHConnectionTarget ->
  -- | Local path
  Text ->
  -- | Remote path
  Text ->
  IO Text
scpUpload sshOpts local remote =
  runBash [i|#{scp} -r #{local} #{sshString}:#{remote}|]
 where
  sshString = sshOpts.username <> "@" <> sshOpts.host
  scp :: Text = case sshOpts.password of
    Nothing -> "scp -o PasswordAuthentication=no -o StrictHostKeychecking=no"
    Just pass -> [i|sshpass -p #{pass} scp -o StrictHostKeychecking=no|]

validateSSH :: SSHConnectionTarget -> IO Bool
validateSSH sshOpts = do
  textOut <- runSSHCommand sshOpts "echo 'hello'"
  return (textOut == "hello\n")

verilogImplForEvals :: (SC.SCType -> Int) -> SC.FunctionDeclaration -> [Evaluation] -> Text
verilogImplForEvals typeWidth scFun evals =
  [__i|
      module top(#{decls});
      #{body}
      endmodule
      |]
 where
  decls = T.intercalate ", " (inputDecls ++ [outputDecl])

  body :: Text
  body
    | not (null inputDecls) =
        [__i|
        always_comb begin case (#{concatInputs})
        #{cases}
        endcase end
          |]
    | (evalHead : _) <- evals =
        [i|assign out = #{comparisonValueAsVerilog (evalHead ^. #output)};|]
    | otherwise =
        [i|assign out = {#{outputWidth}{X}};|]

  inputDecls :: [Text] =
    [ [i|input wire [#{typeWidth t - 1}:0] #{name}|]
    | (t, name) <- scFun.args
    ]

  outputWidth = typeWidth scFun.returnType
  outputDecl :: Text = [i|output reg [#{outputWidth - 1}:0] out|]

  concatInputs :: Text =
    "{" <> T.intercalate ", " [name | (_, name) <- scFun.args] <> "}"

  cases :: Text =
    T.intercalate
      "\n"
      [ let concatInputVals = T.intercalate ", " (map comparisonValueAsVerilog inputs)
         in [i|  {#{concatInputVals}}: out = #{comparisonValueAsVerilog output};|]
      | Evaluation{inputs, output} <- evals
      ]
