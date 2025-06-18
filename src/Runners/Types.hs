{-# LANGUAGE DataKinds #-}
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

newtype ExperimentRunner = ExperimentRunner
  { run :: Experiment -> IO ExperimentResult
  }

data SSHConnectionTarget = SSHConnectionTarget
  { host :: Text
  , username :: Text
  , password :: Maybe Text
  }

makeFieldLabelsNoPrefix ''SSHConnectionTarget
