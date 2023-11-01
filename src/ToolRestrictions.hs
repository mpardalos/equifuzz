{-# HLINT ignore "Use const" #-}
module ToolRestrictions where

import GenSystemC.Config
  ( GenMods (..),
    TransformationFlags (..),
    TypeOperationsMod,
    allTransformations,
  )
import Optics
import SystemC qualified as SC

vcfMods :: GenMods
vcfMods = GenMods {operations, transformations}
  where
    operations e = case e.annotation of
      SC.SCInt {} ->
        composeAll
          [ #constructorInto % #scBigInt .~ False,
            #constructorInto % #scBigUInt .~ False
          ]
      SC.SCUInt {} ->
        composeAll
          [ #constructorInto % #scBigInt .~ False,
            #constructorInto % #scBigUInt .~ False
          ]
      SC.CDouble {} ->
        composeAll
          [ #constructorInto % #cInt .~ False,
            #constructorInto % #cUInt .~ False,
            #assignTo % #cInt .~ False,
            #assignTo % #cUInt .~ False
          ]
      _ -> id

    transformations =
      allTransformations
        { arithmetic = False
        }

jasperMods :: GenMods
jasperMods = GenMods {operations, transformations}
  where
    operations :: TypeOperationsMod
    operations e =
      composeAll
        [ noFixed,
          failingBigIntReductions e,
          missingSubrefReductions e,
          ambiguousAssignments e
        ]

    failingBigIntReductions e = case e.annotation of
      SC.SCBigInt {} -> #reductions %~ filter (`notElem` [SC.ReduceXor, SC.ReduceXNor])
      SC.SCBigUInt {} -> #reductions %~ filter (`notElem` [SC.ReduceXor, SC.ReduceXNor])
      _ -> id

    missingSubrefReductions e = case e.annotation of
      SC.SCIntSubref {} -> #reductions .~ []
      SC.SCUIntSubref {} -> #reductions .~ []
      SC.SCSignedSubref {} -> #reductions .~ []
      SC.SCUnsignedSubref {} -> #reductions .~ []
      _ -> id

    ambiguousAssignments :: TypeOperationsMod
    ambiguousAssignments e = case e.annotation of
      SC.SCIntSubref {} ->
        composeAll
          [ #assignTo % #scUInt .~ False,
            #assignTo % #scInt .~ False
          ]
      SC.SCUIntSubref {} ->
        composeAll
          [ #assignTo % #scUInt .~ False,
            #assignTo % #scInt .~ False
          ]
      SC.SCIntBitref {} ->
        composeAll
          [ #assignTo % #scUInt .~ False,
            #assignTo % #scInt .~ False
          ]
      SC.SCUIntBitref {} ->
        composeAll
          [ #assignTo % #scUInt .~ False,
            #assignTo % #scInt .~ False
          ]
      _ -> id

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
