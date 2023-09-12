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

import Data.Data (Data)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import qualified SystemC as SC
import GenSystemC (BuildOut)

-- | Identifies a sequence of experiments
newtype ExperimentSequenceId = ExperimentSequenceId {uuid :: UUID}
  deriving (Show, Eq, Ord, Generic, Data)

newExperimentSequenceId :: IO ExperimentSequenceId
newExperimentSequenceId = ExperimentSequenceId <$> UUID.nextRandom

-- | Identifies a single experiment within a sequence of experiments
newtype ExperimentId = ExperimentId {uuid :: UUID}
  deriving (Show, Eq, Ord, Generic, Data)

newExperimentId :: IO ExperimentId
newExperimentId = ExperimentId <$> UUID.nextRandom

data Experiment = Experiment
  { experimentId :: ExperimentId,
    -- | True if we expect the modules to be equivalent, False if we expect them not to be
    expectedResult :: Bool,
    design :: SC.FunctionDeclaration BuildOut,
    -- | Used for ordering reductions of the same experiment. Will probably be
    -- the number of transformations used to generate it
    size :: Int,
    -- | Human-readable text describing the design/how it was generated
    -- E.g. The series of transformations that generated it
    longDescription :: Text,
    -- | Value that the design will be compared to
    comparisonValue :: Text
  }
  deriving (Generic, Show, Eq, Ord)

data DesignSource = DesignSource
  { topName :: Text,
    source :: Text
  }
  deriving (Show, Eq, Ord)

data ExperimentResult = ExperimentResult
  { experimentId :: ExperimentId,
    proofFound :: Maybe Bool,
    counterExample :: Maybe Text,
    fullOutput :: Text
  }
  deriving (Show, Generic, Eq)

data ExperimentProgress
  = ExperimentStarted ExperimentSequenceId Experiment
  | ExperimentCompleted ExperimentSequenceId ExperimentResult
  | ExperimentSequenceCompleted ExperimentSequenceId
  deriving (Show)

makeFieldLabelsNoPrefix ''Experiment
makeFieldLabelsNoPrefix ''DesignSource
makeFieldLabelsNoPrefix ''ExperimentResult
makeFieldLabelsNoPrefix ''ExperimentId
makeFieldLabelsNoPrefix ''ExperimentSequenceId
