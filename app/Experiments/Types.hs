{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments.Types where

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
    designSpec :: DesignSource,
    designImpl :: DesignSource
  }
  deriving (Generic, Show)

instance Eq Experiment where
  (==) = (==) `on` view #uuid

instance Ord Experiment where
  compare = compare `on` view #uuid

data DesignLanguage = SystemC | Verilog
  deriving (Show)

data DesignSource = DesignSource
  { language :: DesignLanguage,
    topName :: Text,
    source :: Text
  }
  deriving (Show)

type RunnerInfo = Text

data ExperimentResult = ExperimentResult
  { proofFound :: Maybe Bool,
    counterExample :: Maybe Text,
    fullOutput :: Text,
    runnerInfo :: RunnerInfo,
    uuid :: UUID
  }
  deriving (Show, Eq, Generic, Data)

data ExperimentProgress
  = NewExperiment Experiment
  | BeginRun UUID RunnerInfo
  | RunCompleted ExperimentResult
  | ExperimentCompleted UUID
  deriving (Show)

makeFieldLabelsNoPrefix ''Experiment
makeFieldLabelsNoPrefix ''DesignSource
makeFieldLabelsNoPrefix ''ExperimentResult
