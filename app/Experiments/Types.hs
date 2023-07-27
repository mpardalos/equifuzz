{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments.Types where

import Control.Exception (Exception, SomeException)
import Data.Data (Data)
import Data.Function (on)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix, view)

data Experiment = Experiment
  { uuid :: UUID,
    -- | True if we expect the modules to be equivalent, False if we expect them not to be
    expectedResult :: Bool,
    design :: DesignSource,
    -- | Human-readable text describing the design/how it was generated
    -- E.g. The series of transformations that generated it
    designDescription :: Text,
    -- | Value that the design will be compared to
    comparisonValue :: Text
  }
  deriving (Generic, Show)

instance Eq Experiment where
  (==) = (==) `on` view #uuid

instance Ord Experiment where
  compare = compare `on` view #uuid

data DesignSource = DesignSource
  { topName :: Text,
    source :: Text
  }
  deriving (Show)

data ExperimentResult = ExperimentResult
  { proofFound :: Maybe Bool,
    counterExample :: Maybe Text,
    fullOutput :: Text,
    uuid :: UUID
  }
  deriving (Show, Eq, Generic, Data)

data RunnerError
  = OutOfLicenses
  | RunnerCrashed SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

data ExperimentProgress
  = ExperimentStarted Experiment
  | ExperimentFailed UUID RunnerError
  | ExperimentCompleted ExperimentResult
  deriving (Show)

makeFieldLabelsNoPrefix ''Experiment
makeFieldLabelsNoPrefix ''DesignSource
makeFieldLabelsNoPrefix ''ExperimentResult
