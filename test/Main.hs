module Main where

import Control.Concurrent (getNumCapabilities)
import Control.Exception (SomeException, evaluate)
import Control.Monad (forM_, replicateM_)
import Data.Map qualified as Map
import Experiments (mkSystemCConstantExperiment)
import GHC.Conc (numCapabilities)
import GenSystemC (GenConfig (..), Reducible (..))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import ToolRestrictions (noMods)

genConfig :: GenConfig
genConfig =
  GenConfig
    { growSteps = 30,
      mods = noMods
    }

generateSomeExperiments :: Int -> IO ()
generateSomeExperiments count = replicateM_ count $ do
  experiment <- mkSystemCConstantExperiment genConfig
  evaluate =<< experiment.value
  -- | Just try a few arbitrary reductions
  forM_ [(0, 10), (10, 20), (20, 29), (10, 12), (5, 25)] $ \bounds -> do
    evaluate =<< (experiment.reductions Map.! bounds).value

main :: IO ()
main = do
  threadCount <- getNumCapabilities

  let totalExperiments = 10
      experimentsPerThread = totalExperiments `div` threadCount

  replicateM_ threadCount $
    generateSomeExperiments experimentsPerThread
