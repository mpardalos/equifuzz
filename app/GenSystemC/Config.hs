module GenSystemC.Config (TypeOperationsMod, GenConfig (..)) where

import SystemC qualified as SC

type TypeOperationsMod = SC.SCType -> SC.Operations -> SC.Operations

data GenConfig = GenConfig
  { growSteps :: Int,
    operationsMod :: TypeOperationsMod
  }
