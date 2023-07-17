{-# LANGUAGE DuplicateRecordFields #-}

module GenSystemC.Reduce where

import Control.Monad (foldM)
import Control.Monad.State.Strict (runState)
import Data.Text (Text)
import GenSystemC.Transformations
import SystemC qualified as SC

data GenerateProcess = GenerateProcess
  { seed :: SC.Expr BuildOut,
    transformations :: [Transformation],
    reductions :: Int -> [GenerateProcess]
  }

mkGenerateProcess :: SC.Expr BuildOut -> [Transformation] -> GenerateProcess
mkGenerateProcess seed transformations =
  GenerateProcess
    { seed,
      transformations,
      reductions = mkReductions seed transformations
    }

mkReductions :: SC.Expr BuildOut -> [Transformation] -> Int -> [GenerateProcess]
mkReductions seed fullTransformations window =
  [ GenerateProcess
      { seed,
        transformations,
        reductions = mkReductions seed transformations
      }
    | transformations <- windowsOf window fullTransformations
  ]

windowsOf :: Int -> [a] -> [[a]]
windowsOf size initLst = take (length initLst) (go initLst)
  where
    go xs = take size (cycle xs) : go (tail (cycle xs))
