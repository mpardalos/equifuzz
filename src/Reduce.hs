{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reduce where

import Data.List (nub, transpose)
import Experiments (Experiment (..), generateProcessToExperiment)
import GenSystemC (GenerateProcess (..), Transformation (..))
import Optics (over)
import Safe (headDef, initSafe)
import SystemC qualified as SC

class HasReductions a where
  type Reduced a
  type Reduced a = a

  -- Get possible reductions for this value, in the order they should be
  -- evaluated at
  mkReductions :: a -> [Reduced a]

-- | All possible ways to remove a chunk of N elements from the list
-- (non-overlapping chunks)
removingChunk :: [a] -> Int -> [[a]]
removingChunk xs 0 = [xs]
removingChunk [] _ = []
removingChunk xs n
  | n > length xs = []
  | otherwise =
      let thisChunk = take n xs
          rest = drop n xs
       in rest : map (thisChunk ++) (rest `removingChunk` n)

instance HasReductions Int where
  mkReductions 0 = []
  mkReductions 1 = []
  mkReductions n = [n `div` 2]

instance HasReductions SC.SCType where
  mkReductions (SC.SCInt n) = SC.SCInt <$> mkReductions n
  mkReductions (SC.SCUInt n) = SC.SCUInt <$> mkReductions n
  mkReductions (SC.SCBigInt n) = SC.SCBigInt <$> mkReductions n
  mkReductions (SC.SCBigUInt n) = SC.SCBigUInt <$> mkReductions n
  mkReductions (SC.SCBV{width}) = SC.SCBV <$> mkReductions width
  mkReductions (SC.SCLV{width}) = SC.SCLV <$> mkReductions width
  mkReductions _ = []

instance HasReductions SC.Expr where
  -- TODO: Reduce variables to constants
  mkReductions (SC.Variable _ _) = []
  mkReductions _ = []

instance HasReductions Transformation where
  mkReductions (CastWithAssignment t) =
    FunctionalCast t : (CastWithAssignment <$> mkReductions t)
  mkReductions (FunctionalCast t) =
    FunctionalCast <$> mkReductions t
  mkReductions (Range _ _) = []
  mkReductions (Arithmetic op e) = Arithmetic <$> [SC.Plus, op] <*> mkReductions e
  mkReductions (UseAsCondition t f) = UseAsCondition <$> mkReductions t <*> mkReductions f
  mkReductions (BitSelect _) = []
  mkReductions (ApplyMethod _) = []
  mkReductions (ApplyUnaryOp _) = []

reduceIndividualTransformations :: (Reduced a ~ a, HasReductions a) => [a] -> [[a]]
reduceIndividualTransformations transformations =
  let
    possibleReductions = map mkReductions transformations
   in
    if all null possibleReductions
      -- We need this case because otherwise we get a non-reduced option when
      -- there are no reducible transformations
      then []
      -- Need to keep the original transformation for non-reducible ones
      else
      -- The last case here is non-reduced, just skip it
        initSafe $
          cartesian
            [ rs ++ [t]
            | (t, rs) <- zip transformations possibleReductions
            ]

cartesian :: [[a]] -> [[a]]
cartesian [] = [[]]
cartesian (xs : xss) = do
  y <- xs
  ys <- cartesian xss
  return (y : ys)

instance HasReductions [Transformation] where
  mkReductions transformations =
    let
      removingTransformations =
        [ reduced
        | removeCount <- nub [length transformations `div` 2, 2, 1]
        , reduced <- transformations `removingChunk` removeCount
        , length reduced < length transformations
        , length transformations == 1 || length reduced > 0
        ]
     in
      removingTransformations ++ reduceIndividualTransformations transformations

instance HasReductions GenerateProcess where
  mkReductions process =
    [ process{transformations}
    | transformations :: [Transformation] <-
        mkReductions process.transformations
    ]

instance HasReductions Experiment where
  type Reduced Experiment = IO Experiment
  mkReductions Experiment{generateProcess} =
    generateProcessToExperiment <$> mkReductions generateProcess
