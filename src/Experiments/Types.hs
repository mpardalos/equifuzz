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

import Data.Char (intToDigit)
import Data.Data (Data)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Numeric (readBin, showIntAtBase)
import Optics (makeFieldLabelsNoPrefix)
import Prettyprinter (Pretty (..), (<+>))
import Safe (fromJustNote)
import SystemC qualified as SC

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

-- | literal value represented as (big-endian) string of 0s and 1s
newtype ComparisonValue = UnsafeComparisonValue Text
  deriving newtype (Eq, Ord, Show)

mkComparisonValueWord ::
  -- | Width
  Int ->
  -- | Value
  Word ->
  ComparisonValue
mkComparisonValueWord w v = mkComparisonValueWithWidth w (T.pack (showIntAtBase 2 intToDigit v ""))

mkComparisonValue :: Text -> ComparisonValue
mkComparisonValue t
  | Just c <- T.find (`notElem` ['0', '1']) t =
      error ("Unexpected character in comparison value: " ++ show c)
  | otherwise = UnsafeComparisonValue t

mkComparisonValueWithWidth :: Int -> Text -> ComparisonValue
mkComparisonValueWithWidth w t
  | T.length t < w = mkComparisonValue (T.replicate (w - T.length t) "0" <> t)
  | otherwise = mkComparisonValue (T.takeEnd w t)

comparisonValueRaw :: ComparisonValue -> Text
comparisonValueRaw (UnsafeComparisonValue t) = t

comparisonValueAsC :: ComparisonValue -> Text
comparisonValueAsC (UnsafeComparisonValue t) = "0b" <> t

comparisonValueAsVerilog :: ComparisonValue -> Text
comparisonValueAsVerilog (UnsafeComparisonValue t) =
  T.pack (show (T.length t)) <> "'b" <> t

readBinMay :: (Eq a, Num a) => String -> Maybe a
readBinMay s =
  case readBin s of
    [(n, "")] -> Just n
    _ -> Nothing

comparisonValueAsSC :: SC.SCType -> ComparisonValue -> SC.Expr
comparisonValueAsSC t val =
  SC.Constant
    t
    ( val
        & comparisonValueRaw
        & T.unpack
        & readBinMay
        & fromJustNote "Invalid ComparisonValue"
    )

data Evaluation = Evaluation
  { inputs :: [ComparisonValue]
  , output :: ComparisonValue
  }
  deriving (Generic, Show, Eq, Ord)

instance Pretty ComparisonValue where
  pretty = pretty . comparisonValueAsVerilog

instance Pretty Evaluation where
  pretty Evaluation{inputs, output} =
    pretty inputs <+> " -> " <+> pretty output

data Experiment = Experiment
  { experimentId :: ExperimentId
  , expectedResult :: Bool
  -- ^ True if we expect the modules to be equivalent, False if we expect them not to be
  , scDesign :: SC.FunctionDeclaration
  , verilogDesign :: Text
  , size :: Int
  -- ^ Used for ordering reductions of the same experiment. Will probably be
  -- the number of transformations used to generate it
  , longDescription :: Text
  -- ^ Human-readable text describing the design/how it was generated
  -- E.g. The series of transformations that generated it
  , knownEvaluations :: [Evaluation]
  -- ^ Input vectors and matching results at which the design will be evaluated
  }
  deriving (Generic, Show, Eq, Ord)

data DesignSource = DesignSource
  { topName :: Text
  , source :: Text
  }
  deriving (Show, Eq, Ord)

data ExperimentResult = ExperimentResult
  { experimentId :: ExperimentId
  , proofFound :: Maybe Bool
  , counterExample :: Maybe Text
  , fullOutput :: Text
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
