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

jasperMods :: GenMods
jasperMods = GenMods {operations, transformations}
  where
    operations t = noFixed . failingBigIntReductions t

    failingBigIntReductions SC.SCBigInt {} =
      #reductions %~ filter (`notElem` [SC.ReduceXor, SC.ReduceXNor])
    failingBigIntReductions SC.SCBigUInt {} =
      #reductions %~ filter (`notElem` [SC.ReduceXor, SC.ReduceXNor])
    failingBigIntReductions _ = id

    noFixed = composeAll
      [ #constructorInto % #scFixed .~ False,
        #constructorInto % #scUFixed .~ False,
        #assignTo % #scFixed .~ False,
        #assignTo % #scUFixed .~ False
      ]
    transformations = allTransformations

noMods :: GenMods
noMods =
  GenMods
    { operations = const id,
      transformations = allTransformations
    }

composeAll :: Foldable t => t (c -> c) -> c -> c
composeAll = foldl (.) id
