{-# HLINT ignore "Use const" #-}
module ToolRestrictions where

import GenSystemC.Config
  ( GenMods (..),
    TransformationFlags (..),
    allTransformations,
  )
import Optics
import SystemC qualified as SC

vcfMods :: GenMods
vcfMods = GenMods {operations, transformations}
  where
    operations SC.SCInt {} =
      composeAll
        [ #constructorInto % #scBigInt .~ False,
          #constructorInto % #scBigUInt .~ False
        ]
    operations SC.SCUInt {} =
      composeAll
        [ #constructorInto % #scBigInt .~ False,
          #constructorInto % #scBigUInt .~ False
        ]
    operations SC.CDouble {} =
      composeAll
        [ #constructorInto % #cInt .~ False,
          #constructorInto % #cUInt .~ False,
          #assignTo % #cInt .~ False,
          #assignTo % #cUInt .~ False
        ]
    operations _ = id

    transformations =
      allTransformations
        { arithmetic = False
        }

noMods :: GenMods
noMods =
  GenMods
    { operations = const id,
      transformations = allTransformations
    }

composeAll :: Foldable t => t (c -> c) -> c -> c
composeAll = foldl (.) id
