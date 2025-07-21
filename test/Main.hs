module Main where

import Control.Concurrent.Async (forConcurrently_, replicateConcurrently_)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM.TSem
import Control.Exception (bracket_, evaluate)
import Control.Monad (forever, replicateM_)
import Control.Monad.Random (evalRandIO, join, uniform, void)
import Experiments (Experiment (..), genSystemCConstantExperiment)
import GHC.Conc (atomically)
import GenSystemC (GenConfig (..))
import Reduce (HasReductions (mkReductions))
import System.IO (hPutStr, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import ToolRestrictions (noMods)

genConfig :: GenConfig
genConfig =
  GenConfig
    { growSteps = 30
    , transformationAllowed = noMods
    , evaluations = 3
    }

totalExperiments :: Int
totalExperiments = 10

-- | How many reductions to try at each reduction size
reductionCount :: Int
reductionCount = 5

totalRuns :: Int
totalRuns = totalExperiments * (1 + reductionCount)

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

runCompletedProgress :: IO ()
runCompletedProgress = do
  modifyMVar_ runsCompleteVar (return . (1 +))
  runsComplete <- readMVar runsCompleteVar
  withIOLock $ do
    hPutStr stderr "\r"
    hPutStr stderr "["
    hPutStr stderr (progressBar 50 runsComplete totalRuns)
    hPutStr stderr "]"
    hPutStr stderr (printf "[%3d/%-3d]" runsComplete totalRuns)

runCompletedUnbounded :: IO ()
runCompletedUnbounded = do
  modifyMVar_ runsCompleteVar (return . (1 +))
  runsComplete <- readMVar runsCompleteVar
  withIOLock $ do
    hPutStr stderr "\r"
    hPutStr stderr (printf "%d Runs completed" runsComplete)

concurrency :: Int
concurrency = 3

runForever :: IO ()
runForever = do
  replicateConcurrently_ concurrency $ forever $ do
    experiment <- genSystemCConstantExperiment genConfig
    void $ evaluate experiment.verilogDesign
    runCompletedUnbounded
    let reductions = mkReductions experiment
    replicateM_ reductionCount $ do
      reducedExperiment <- join $ evalRandIO (uniform reductions)
      void $ evaluate reducedExperiment.verilogDesign
      runCompletedUnbounded

runLimited :: IO ()
runLimited = do
  count <- atomically $ newTSem (fromIntegral concurrency)
  forConcurrently_ [1 :: Int .. totalExperiments] $ \_experimentIdx -> do
    atomically $ waitTSem count
    experiment <- genSystemCConstantExperiment genConfig
    runCompletedProgress
    let reductions = mkReductions experiment
    replicateM_ reductionCount $ do
      reducedExperiment <- join $ evalRandIO (uniform reductions)
      void $ evaluate reducedExperiment
      runCompletedProgress
    atomically $ signalTSem count
  hPutStrLn stderr ""

main :: IO ()
main = runForever
