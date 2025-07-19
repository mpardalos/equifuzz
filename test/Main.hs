module Main where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM.TSem
import Control.Exception (bracket_, evaluate)
import Control.Monad (forM_, replicateM_)
import Data.Map qualified as Map
import Experiments (genSystemCConstantExperiment)
import GHC.Conc (atomically)
import GenSystemC (GenConfig (..))
import System.IO (hPutStr, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import ToolRestrictions (noMods)
import Reduce (HasReductions(mkReductions))
import Control.Monad.Random (evalRandIO, uniform, join, void)

genConfig :: GenConfig
genConfig =
  GenConfig
    { growSteps = 30
    , transformationAllowed = noMods
    , evaluations = 3
    }

totalExperiments :: Int
totalExperiments = 10

reductionSizes :: [Int]
reductionSizes = [2, 5, 10]

-- | How many reductions to try at each reduction size
reductionCount :: Int
reductionCount = 2

totalRuns :: Int
totalRuns = totalExperiments * (1 + reductionCount * length reductionSizes)

ioLock :: MVar ()
{-# NOINLINE ioLock #-}
ioLock = unsafePerformIO $ newMVar ()

withIOLock :: IO a -> IO a
withIOLock = bracket_ (takeMVar ioLock) (putMVar ioLock ())

runsCompleteVar :: MVar Int
{-# NOINLINE runsCompleteVar #-}
runsCompleteVar = unsafePerformIO $ newMVar 0

progressBar :: Int -> Int -> Int -> String
progressBar totalSegments complete total =
  let
    fullSegments = (totalSegments * complete) `div` total
    emptySegments = totalSegments - fullSegments
   in
    replicate fullSegments '=' <> replicate emptySegments ' '

runCompleted :: IO ()
runCompleted = do
  modifyMVar_ runsCompleteVar (return . (1 +))
  runsComplete <- readMVar runsCompleteVar
  withIOLock $ do
    hPutStr stderr "\r"
    hPutStr stderr "["
    hPutStr stderr (progressBar 50 runsComplete totalRuns)
    hPutStr stderr "]"
    hPutStr stderr (printf "[%3d/%-3d]" runsComplete totalRuns)

main :: IO ()
main = do
  capabilities <- getNumCapabilities
  count <- atomically $ newTSem (fromIntegral capabilities)
  forConcurrently_ [1 :: Int .. totalExperiments] $ \_experimentIdx -> do
    atomically $ waitTSem count
    experiment <- genSystemCConstantExperiment genConfig
    runCompleted
    let reductions = mkReductions experiment
    forM_ reductionSizes $ \reductionSize -> do
      let reducedCandidates = reductions Map.! reductionSize
      replicateM_ reductionCount $ do
        reducedExperiment <- join $ evalRandIO (uniform reducedCandidates)
        void $ evaluate reducedExperiment
        runCompleted
    atomically $ signalTSem count
  hPutStrLn stderr ""
