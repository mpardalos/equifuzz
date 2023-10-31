{-# HLINT ignore "Use const" #-}
module ToolRestrictions where

import GenSystemC.Config
  ( GenMods (..),
    TransformationFlags (..),
    allTransformations, TypeOperationsMod,
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
    operations t =
      composeAll
        [ noFixed,
          failingBigIntReductions t,
          missingSubrefReductions t,
          ambiguousAssignments t
        ]

    failingBigIntReductions SC.SCBigInt {} =
      #reductions %~ filter (`notElem` [SC.ReduceXor, SC.ReduceXNor])
    failingBigIntReductions SC.SCBigUInt {} =
      #reductions %~ filter (`notElem` [SC.ReduceXor, SC.ReduceXNor])
    failingBigIntReductions _ = id

    missingSubrefReductions SC.SCIntSubref {} = #reductions .~ []
    missingSubrefReductions SC.SCUIntSubref {} = #reductions .~ []
    missingSubrefReductions SC.SCSignedSubref {} = #reductions .~ []
    missingSubrefReductions SC.SCUnsignedSubref {} = #reductions .~ []
    missingSubrefReductions _ = id

    ambiguousAssignments :: TypeOperationsMod
    ambiguousAssignments SC.SCIntSubref {} =
      composeAll
      [ #assignTo % #scUInt .~ False
      , #assignTo % #scInt .~ False
      ]
    ambiguousAssignments SC.SCUIntSubref {} =
      composeAll
      [ #assignTo % #scUInt .~ False
      , #assignTo % #scInt .~ False
      ]
    ambiguousAssignments SC.SCIntBitref {} =
      composeAll
      [ #assignTo % #scUInt .~ False
      , #assignTo % #scInt .~ False
      ]
    ambiguousAssignments SC.SCUIntBitref {} =
      composeAll
      [ #assignTo % #scUInt .~ False
      , #assignTo % #scInt .~ False
      ]
    ambiguousAssignments _ = id

    noFixed =
      composeAll
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
