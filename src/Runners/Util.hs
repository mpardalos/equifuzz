{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Runners.Util where

import Control.Monad (forM_, void)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Runners.Types (SSHConnectionTarget (..))
import Shelly (Sh, (</>))
import Shelly qualified as Sh
import Util (bashExec)

createRemoteExperimentDir :: SSHConnectionTarget -> Text -> [(Text, Text)] -> Sh ()
createRemoteExperimentDir sshOpts remoteExperimentDir files = do
  localExperimentDir <- T.strip <$> Sh.run "mktemp" ["-d"]

  forM_ files $ \(filename, content) ->
    Sh.writefile (localExperimentDir </> filename) content

  void $ runSSHCommand sshOpts [i|mkdir -p #{remoteExperimentDir}/|]
  void $ scpUpload sshOpts [i|#{localExperimentDir}/*|] [i|#{remoteExperimentDir}/|]

runSSHCommand :: SSHConnectionTarget -> Text -> Sh Text
runSSHCommand sshOpts command = bashExec [i|#{ssh} #{sshString} '#{command}'|]
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
  Sh Text
scpDownload sshOpts remote local =
  bashExec [i|#{scp} #{sshString}:#{remote} #{local}|]
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
  Sh Text
scpUpload sshOpts local remote =
  bashExec [i|#{scp} -r #{local} #{sshString}:#{remote}|]
 where
  sshString = sshOpts.username <> "@" <> sshOpts.host
  scp :: Text = case sshOpts.password of
    Nothing -> "scp -o PasswordAuthentication=no -o StrictHostKeychecking=no"
    Just pass -> [i|sshpass -p #{pass} scp -o StrictHostKeychecking=no|]

validateSSH :: SSHConnectionTarget -> Sh Bool
validateSSH sshOpts = Sh.silently $ do
  textOut <- runSSHCommand sshOpts "echo 'hello'"
  return (textOut == "hello\n")
