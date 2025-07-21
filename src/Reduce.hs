{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reduce where

import Experiments (Experiment (..), generateProcessToExperiment)
import GenSystemC (GenerateProcess (..))

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

instance HasReductions GenerateProcess where
  mkReductions (GenerateProcess cfg seed transformations) =
    map
      (GenerateProcess cfg seed)
      ( [ reduced
        | removeCount <- [length transformations `div` 2, 2, 1]
        , reduced <- removingChunk removeCount transformations
        ]
      )

instance HasReductions Experiment where
  type Reduced Experiment = IO Experiment
  mkReductions Experiment{generateProcess} =
    generateProcessToExperiment generateProcess.cfg <$> mkReductions generateProcess
