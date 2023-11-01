module GenSystemC.Config where

import SystemC qualified as SC

type TypeOperationsMod = SC.SCType -> SC.Operations -> SC.Operations

data TransformationFlags = TransformationFlags
  { castWithAssignment :: Bool,
    functionalCast :: Bool,
    range :: Bool,
    arithmetic :: Bool,
    useAsCondition :: Bool,
    bitSelect :: Bool,
    applyReduction :: Bool,
    applyUnaryOp :: Bool
  }

allTransformations :: TransformationFlags
allTransformations =
  TransformationFlags
    { castWithAssignment = True,
      functionalCast = True,
      range = True,
      arithmetic = True,
      useAsCondition = True,
      bitSelect = True,
      applyReduction = True,
      applyUnaryOp = True
    }

data GenMods = GenMods
  { operations :: TypeOperationsMod,
    transformations :: TransformationFlags
  }

data GenConfig = GenConfig
  { growSteps :: Int,
    mods :: GenMods
  }
