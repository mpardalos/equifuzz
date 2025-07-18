module GenSystemC.Config where

import SystemC qualified as SC
import Text.Show.Functions()

type OperationsMod = SC.Expr -> SC.Operations -> SC.Operations

data TransformationFlags = TransformationFlags
  { castWithAssignment :: Bool
  , functionalCast :: Bool
  , range :: Bool
  , arithmetic :: Bool
  , useAsCondition :: Bool
  , bitSelect :: Bool
  , applyMethod :: Bool
  , applyUnaryOp :: Bool
  }
  deriving (Show)

allTransformations :: TransformationFlags
allTransformations =
  TransformationFlags
    { castWithAssignment = True
    , functionalCast = True
    , range = True
    , arithmetic = True
    , useAsCondition = True
    , bitSelect = True
    , applyMethod = True
    , applyUnaryOp = True
    }

data GenMods = GenMods
  { operations :: OperationsMod
  , transformations :: TransformationFlags
  , inputs :: Bool
  }
  deriving (Show)

data GenConfig = GenConfig
  { growSteps :: Int
  , mods :: GenMods
  , evaluations :: Int
  }
  deriving (Show)
