{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reduce where

import Data.Function ((&))
import Experiments (Experiment (..), generateProcessToExperiment)
import GenSystemC (GenerateProcess (..), Transformation (..))
import Safe (headDef)
import SystemC qualified as SC

class HasReductions a where
  type Reduced a
  type Reduced a = a

  -- Get possible reductions for this value, in the order they should be
  -- evaluated at
  mkReductions :: a -> [Reduced a]

-- | All possible ways to remove a chunk of N elements from the list
-- (non-overlapping chunks)
removingChunk :: Int -> [a] -> [[a]]
removingChunk 0 xs = [xs]
removingChunk _ [] = []
removingChunk n xs
  | n > length xs = []
  | otherwise =
      let thisChunk = take n xs
          rest = drop n xs
       in rest : map (thisChunk ++) (removingChunk n rest)

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
  mkReductions (SC.Variable t n) = []
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

instance HasReductions GenerateProcess where
  mkReductions process@(GenerateProcess{transformations}) =
    let
      removingTransformations =
        [ reduced
        | removeCount <- [length transformations `div` 2, 2, 1]
        , reduced <- removingChunk removeCount transformations
        ]
      reducingTransformations =
        [map (\t -> headDef t (mkReductions t)) transformations] -- Reduce all transformations
     in
      map (\ts -> process{transformations = ts}) (removingTransformations ++ reducingTransformations)

instance HasReductions Experiment where
  type Reduced Experiment = IO Experiment
  mkReductions Experiment{generateProcess} =
    generateProcessToExperiment generateProcess.cfg <$> mkReductions generateProcess
