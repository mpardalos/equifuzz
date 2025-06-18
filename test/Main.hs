module Main where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently, forConcurrently_)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM.TSem
import Control.Exception (SomeException, bracket_, evaluate)
import Control.Monad (forM_, replicateM_)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Experiments (mkSystemCConstantExperiment)
import GHC.Conc (atomically, numCapabilities)
import GenSystemC (GenConfig (..), Reducible (..))
import System.Console.ANSI (hClearLine, hSetCursorColumn)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import ToolRestrictions (noMods)

genConfig :: GenConfig
genConfig =
  GenConfig
    { growSteps = 30
    , mods = noMods
    }

totalExperiments :: Int
totalExperiments = 10

reductions :: [(Int, Int)]
reductions = [(0, 10), (10, 20), (20, 29), (10, 12), (5, 25)]

totalRuns :: Int
totalRuns = totalExperiments * (1 + length reductions)

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
    reducible <- mkSystemCConstantExperiment genConfig
    _experiment <- evaluate =<< reducible.value
    runCompleted
    forM_ (zip [1 :: Int ..] reductions) $ \(_reductionIdx, bounds) -> do
      reducible' <- evaluate (reducible.reductions Map.! bounds)
      _experiment <- evaluate =<< reducible'.value
      runCompleted
    atomically $ signalTSem count
  hPutStrLn stderr ""
