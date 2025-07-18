{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reduce where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Experiments (Experiment (..), generateProcessToExperiment)
import GenSystemC (GenerateProcess (..))

class HasReductions a where
  type Reduced a
  type Reduced a = a
  mkReductions :: a -> Map Int [Reduced a]
  getSize :: a -> Int

splitN :: Int -> [a] -> [[a]]
splitN 0 [] = [[]]
splitN _ [] = []
splitN n xs = take n xs : splitN n (drop n xs)

instance HasReductions GenerateProcess where
  mkReductions (GenerateProcess cfg seed transformations) =
    let
      removeTransformations =
        Map.fromSet
          ( \size ->
              [ GenerateProcess cfg seed reduced
              | reduced <- splitN size transformations
              ]
          )
          (Set.fromList [0 .. length transformations - 1])
     in
      removeTransformations

  getSize GenerateProcess{transformations} = length transformations

instance HasReductions Experiment where
  type Reduced Experiment = IO Experiment
  getSize Experiment{generateProcess} = getSize generateProcess
  mkReductions Experiment{generateProcess} =
    map (generateProcessToExperiment generateProcess.cfg) <$> mkReductions generateProcess
