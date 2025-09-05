{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (replicateM_)
import Data.Text.IO qualified as T
import Experiments (
  Experiment (..),
  genSystemCConstantExperiment,
  showEvaluation,
 )
import GenSystemC (GenConfig (..))
import Safe (readMay)
import System.Environment (getArgs)
import System.Exit (die)
import ToolRestrictions (noMods)

genConfig :: GenConfig
genConfig =
  GenConfig
    { growSteps = 10
    , genMods = noMods
    , evaluations = 5
    }

main :: IO ()
main = do
  count <-
    getArgs >>= \case
      [readMay -> Just count] -> pure count
      [] -> pure 1
      _ -> die "Invalid arguments"

  replicateM_ count $ do
    Experiment{scSignature, scDesign, verilogDesign, knownEvaluations} <-
      genSystemCConstantExperiment genConfig
    T.putStrLn scDesign
    putStrLn "---------"
    T.putStrLn verilogDesign
    putStrLn "---------"
    mapM_
      (T.putStrLn . showEvaluation scSignature)
      knownEvaluations
