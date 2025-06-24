{-# HLINT ignore "Use const" #-}
module ToolRestrictions where

import Data.Map qualified as Map
import GenSystemC.Config (
  GenMods (..),
  OperationsMod,
  TransformationFlags (..),
  allTransformations,
 )
import Optics
import SystemC qualified as SC

vcfMods :: GenMods
vcfMods = GenMods{operations, transformations, inputs = True}
 where
  operations e = case e.annotation of
    SC.SCInt{} ->
      composeAll
        [ #constructorInto % #scBigInt .~ False
        , #constructorInto % #scBigUInt .~ False
        ]
    SC.SCUInt{} ->
      composeAll
        [ #constructorInto % #scBigInt .~ False
        , #constructorInto % #scBigUInt .~ False
        ]
    SC.CDouble{} ->
      composeAll
        [ #constructorInto % #cInt .~ False
        , #constructorInto % #cUInt .~ False
        , #assignTo % #cInt .~ False
        , #assignTo % #cUInt .~ False
        ]
    _ -> id

  transformations =
    allTransformations
      { arithmetic = False
      }

jasperMods :: GenMods
jasperMods = GenMods{operations, transformations, inputs = True}
 where
  operations :: OperationsMod
  operations e =
    composeAll
      [ noFixed e
      , noLogic e
      , failingBigIntReductions e
      , missingSubrefReductions e
      , ambiguousAssignments e
      , noSingleBitSelections e
      , noBoolToDouble e
      ]

  failingBigIntReductions e = case e.annotation of
    SC.SCBigInt{} -> #methods %~ Map.delete SC.ReduceXor . Map.delete SC.ReduceXNor
    SC.SCBigUInt{} -> #methods %~ Map.delete SC.ReduceXor . Map.delete SC.ReduceXNor
    _ -> id

  missingSubrefReductions e = case e.annotation of
    SC.SCIntSubref{} -> #methods .~ Map.empty
    SC.SCUIntSubref{} -> #methods .~ Map.empty
    SC.SCSignedSubref{} -> #methods .~ Map.empty
    SC.SCUnsignedSubref{} -> #methods .~ Map.empty
    _ -> id

  ambiguousAssignments :: OperationsMod
  ambiguousAssignments e = case e.annotation of
    SC.SCIntSubref{} ->
      composeAll
        [ #assignTo % #scUInt .~ False
        , #assignTo % #scInt .~ False
        ]
    SC.SCUIntSubref{} ->
      composeAll
        [ #assignTo % #scUInt .~ False
        , #assignTo % #scInt .~ False
        ]
    SC.SCIntBitref{} ->
      composeAll
        [ #assignTo % #scUInt .~ False
        , #assignTo % #scInt .~ False
        ]
    SC.SCUIntBitref{} ->
      composeAll
        [ #assignTo % #scUInt .~ False
        , #assignTo % #scInt .~ False
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
            [ #partSelect .~ Nothing
            , #bitSelect .~ Nothing
            ]
    SC.SCUInt w
      | w <= 1 ->
          composeAll
            [ #partSelect .~ Nothing
            , #bitSelect .~ Nothing
            ]
    SC.SCBigInt w
      | w <= 1 ->
          composeAll
            [ #partSelect .~ Nothing
            , #bitSelect .~ Nothing
            ]
    SC.SCBigUInt w
      | w <= 1 ->
          composeAll
            [ #partSelect .~ Nothing
            , #bitSelect .~ Nothing
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
        [ #constructorInto % #cDouble .~ False
        , #assignTo % #cDouble .~ False
        ]
    _ -> id

  transformations = allTransformations

slecMods :: GenMods
slecMods = GenMods{operations, transformations, inputs = True}
 where
  operations :: OperationsMod
  operations e =
    composeAll
      [ failingCasts e
      , noFixed e -- The assignment operator on sc_(u)fixed is broken
      ]

  failingCasts e = case e.annotation of
    SC.SCUInt{} ->
      composeAll
        [ #constructorInto % #cDouble .~ False
        ]
    SC.CDouble{} ->
      composeAll
        [ #constructorInto % #scUInt .~ False
        , #constructorInto % #scInt .~ False
        , #constructorInto % #scBigUInt .~ False
        , #constructorInto % #scBigInt .~ False
        , #assignTo % #scBigUInt .~ False
        , #assignTo % #scBigInt .~ False
        , #assignTo % #scInt .~ False
        , #assignTo % #scUInt .~ False
        ]
    _otherType -> id

  transformations = allTransformations

noFixed :: OperationsMod
noFixed _expr =
  composeAll
    [ #constructorInto % #scFixed .~ False
    , #constructorInto % #scUFixed .~ False
    , #assignTo % #scFixed .~ False
    , #assignTo % #scUFixed .~ False
    ]

noLogic :: OperationsMod
noLogic _expr =
  composeAll
    [ #constructorInto % #scLogic .~ False
    , #constructorInto % #scLogic .~ False
    , #assignTo % #scLogic .~ False
    , #assignTo % #scLogic .~ False
    ]

noMods :: GenMods
noMods =
  GenMods
    { operations = const id
    , transformations = allTransformations
    , inputs = True
    }

composeAll :: Foldable t => t (c -> c) -> c -> c
composeAll = foldl (.) id
