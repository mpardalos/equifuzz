module Main where

import Control.Concurrent (getNumCapabilities)
import Control.Exception (SomeException, evaluate)
import Control.Monad (forM_, replicateM_)
import Data.Map qualified as Map
import Experiments (mkSystemCConstantExperiment)
import GHC.Conc (numCapabilities)
import GenSystemC (GenConfig (..), Reducible (..))
import System.IO (hPutStrLn, stderr, hFlush)
import Text.Printf (printf)
import ToolRestrictions (noMods)

genConfig :: GenConfig
genConfig =
  GenConfig
    { growSteps = 30,
      mods = noMods
    }

totalExperiments :: Int
totalExperiments = 10

reductions :: [(Int, Int)]
reductions = [(0, 10), (10, 20), (20, 29), (10, 12), (5, 25)]

progress :: Int -> Maybe Int -> String -> IO ()
progress experiment mReduction message = do
  hPutStrLn stderr (case mReduction of
    Nothing -> printf "[%d/%d] %s" experiment totalExperiments message
    Just reduction -> printf "[%d/%d] (%d/%d) %s" experiment totalExperiments reduction (length reductions) message)

main :: IO ()
main = forM_ [1..totalExperiments] $ \idx -> do
  progress idx Nothing "Generating experiment"
  experiment <- mkSystemCConstantExperiment genConfig
  progress idx Nothing "Running experiment"
  evaluate =<< experiment.value
  progress idx Nothing "Experiment OK"
  -- | Just try a few arbitrary reductions
  forM_ (zip [1..] reductions) $ \(reductionIdx, bounds) -> do
    progress idx (Just reductionIdx) "Generating reduction"
    reduction <- evaluate (experiment.reductions Map.! bounds)
    progress idx (Just reductionIdx) "Running reduction"
    evaluate =<< reduction.value
  progress idx Nothing "Reductions OK"
