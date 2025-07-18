{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Runners.Common where

import Control.Monad (forM_, void)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Experiments
import Optics (makeFieldLabelsNoPrefix)
import Shelly ((</>))
import Util (mkdir_p, runBash)

default (Text)

type ExperimentRunner = Experiment -> IO ExperimentResult

data EquivalenceCheckerConfig = EquivalenceCheckerConfig
  { name :: Text
  , makeFiles :: Experiment -> [(Text, Text)]
  , runScript :: Text
  , parseOutput :: Experiment -> Text -> ExperimentResult
  }

data SSHConnectionTarget = SSHConnectionTarget
  { host :: Text
  , username :: Text
  , password :: Maybe Text
  }

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

makeFieldLabelsNoPrefix ''EquivalenceCheckerConfig
makeFieldLabelsNoPrefix ''SSHConnectionTarget
