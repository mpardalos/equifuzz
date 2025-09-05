{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module CLI where

import Control.Applicative (Alternative ((<|>)), (<**>), (<|>))
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Optics (isn't, only, (%), _Right)
import Options.Applicative qualified as Opt
import Runners
import System.IO (hFlush, hSetEcho, stdin, stdout)
import Text.Printf (printf)

runOptParse :: String -> Opt.Parser a -> IO a
runOptParse desc p =
  Opt.execParser $
    Opt.info
      (p <**> Opt.helper)
      ( mconcat
          [ Opt.fullDesc
          , Opt.progDesc desc
          ]
      )

data PasswordSource = NoPassword | AskPassword | PasswordGiven Text

data SSHOptions = SSHOptions
  { host :: Text
  , username :: Text
  , passwordSource :: PasswordSource
  , activatePath :: Maybe Text
  }

data RunnerOptions = RunnerOptions
  { sshOptions :: Maybe SSHOptions
  , ecConfig :: EquivalenceCheckerConfig
  }

askPasswordFlag :: Opt.Parser PasswordSource
askPasswordFlag =
  Opt.flag' AskPassword . mconcat $
    [ Opt.long "ask-password"
    , Opt.help "Ask for SSH password to the remote host"
    ]

passwordOption :: Opt.Parser PasswordSource
passwordOption =
  PasswordGiven
    <$> ( Opt.strOption . mconcat $
            [ Opt.long "password"
            , Opt.metavar "PASSWORD"
            , Opt.help "Password to connect to remote host"
            ]
        )

sshOpts :: Opt.Parser (Maybe SSHOptions)
sshOpts = Opt.optional $ do
  host <-
    Opt.strOption . mconcat $
      [ Opt.long "host"
      , Opt.metavar "HOSTNAME"
      , Opt.help "Remote hostname or IP to run equivalence checker on"
      ]
  username <-
    Opt.strOption . mconcat $
      [ Opt.long "username"
      , Opt.metavar "USERNAME"
      , Opt.help "Username to connect to remote host"
      ]
  passwordSource <- askPasswordFlag <|> passwordOption <|> pure NoPassword
  activatePath <-
    Opt.optional . Opt.strOption . mconcat $
      [ Opt.long "activate-script"
      , Opt.metavar "PATH"
      , Opt.help "Script to be sourced on the remote host before running vcf"
      ]
  return SSHOptions{..}

readFecType :: Opt.ReadM EquivalenceCheckerConfig
readFecType = Opt.eitherReader $ \case
  "vcf" -> Right vcFormal
  "jasper" -> Right jasper
  "slec" -> Right slec
  "test" -> Right testRunner
  other -> Left ("FEC '" ++ other ++ "' is unknown")

runnerConfigOpts :: Opt.Parser RunnerOptions
runnerConfigOpts = do
  sshOptions <- sshOpts
  ecConfig <-
    Opt.option readFecType . mconcat $
      [ Opt.long "fec-type"
      , Opt.metavar "TYPE"
      , Opt.help "What FEC type we are running against (vcf|catapult|jasper)"
      ]

  return RunnerOptions{sshOptions, ecConfig}

runnerOptionsToRunner :: RunnerOptions -> IO ExperimentRunner
runnerOptionsToRunner
  RunnerOptions
    { sshOptions
    , ecConfig
    } = do
    -- host,
    -- username,
    -- passwordSource,
    -- activatePath,
    case sshOptions of
      Nothing -> return (runECLocal ecConfig)
      Just SSHOptions{..} -> do
        password <- case passwordSource of
          NoPassword -> pure Nothing
          PasswordGiven pass -> pure (Just pass)
          AskPassword -> Just <$> askPassword
        let sshConnectionTarget = SSHConnectionTarget{username, host, password}
        putStrLn "Validating SSH connection..."
        sshValid <- try @SomeException $ validateSSH sshConnectionTarget
        when (isn't (_Right % only True) sshValid) $
          throwIO (userError "Could not connect to ssh host. Please check the options you provided")
        putStrLn "SSH connection OK"
        return $ runECRemote SSHConnectionTarget{..} activatePath ecConfig

askPassword :: IO Text
askPassword = do
  printf "Your password: "
  hFlush stdout
  hSetEcho stdin False
  password <- T.getLine
  hSetEcho stdin True
  putStr "\n"
  return password
