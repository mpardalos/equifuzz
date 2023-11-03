{-# HLINT ignore "Use const" #-}
module ToolRestrictions where

import GenSystemC.Config
  ( GenMods (..),
    OperationsMod,
    TransformationFlags (..),
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
    operations :: OperationsMod
    operations e =
      composeAll
        [ noFixed,
          failingBigIntReductions e,
          missingSubrefReductions e,
          ambiguousAssignments e,
          noSingleBitSelections e,
          noBoolToDouble e
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

    ambiguousAssignments :: OperationsMod
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

    -- Trying to prevent undefined behaviour in programs like this:
    --     sc_dt::sc_uint<18>(sc_dt::sc_int<1>(-1)[0]);
    -- FIXME: We should detect when the experiment has undefined behaviour and
    -- be able to mark that in the UI
    noSingleBitSelections :: OperationsMod
    noSingleBitSelections e = case e.annotation of
      SC.SCInt w
        | w <= 1 ->
            composeAll
              [ #partSelect .~ Nothing,
                #bitSelect .~ Nothing
              ]
      SC.SCUInt w
        | w <= 1 ->
            composeAll
              [ #partSelect .~ Nothing,
                #bitSelect .~ Nothing
              ]
      _ -> id

    -- Try to avoid bug triggered by this code:
    --   double dut() {
    --       int x0 = 1;
    --       return double(bool(x0));
    --   }
    noBoolToDouble :: OperationsMod
    noBoolToDouble e = case e.annotation of
      SC.CBool ->
        composeAll
          [ #constructorInto % #cDouble .~ False,
            #assignTo % #cDouble .~ False
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
