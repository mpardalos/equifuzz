{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Runners.Types where

import Data.Text (Text)
import Experiments.Types
import Optics (makeFieldLabelsNoPrefix)

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

makeFieldLabelsNoPrefix ''EquivalenceCheckerConfig
makeFieldLabelsNoPrefix ''SSHConnectionTarget
