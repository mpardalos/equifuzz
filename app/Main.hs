{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}

module Main where

import Control.Applicative ((<**>))
import Control.Concurrent (forkFinally, forkIO, newMVar, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever, replicateM_, void)
import Data.Functor ((<&>))
import Data.Text.IO qualified as T
import Data.UUID.V4 qualified as UUID
import Experiments
import Options.Applicative qualified as Opt
import System.Random (getStdRandom, uniformR)
import WebUI (handleProgress, newWebUIState, runWebUI)

experimentThread :: (ExperimentProgress -> IO ()) -> IO ()
experimentThread reportProgress =
  void $
    forkFinally
      ( experimentLoop
          mkSystemCConstantExperiment
          allRunners
          reportProgress
      )
      (const $ experimentThread reportProgress)

webMain :: Bool -> IO ()
webMain test = do
  stateVar <- newMVar =<< newWebUIState
  replicateM_ 10 $
    if test
      then testThread (handleProgress stateVar)
      else experimentThread (handleProgress stateVar)

  runWebUI stateVar

genMain :: IO ()
genMain = do
  Experiment {designSpec, designImpl} <- mkSystemCConstantExperiment
  T.putStrLn designSpec.source
  putStrLn "---------"
  T.putStrLn designImpl.source

main :: IO ()
main =
  parseArgs >>= \case
    Web {test} -> webMain test
    Generate -> genMain

--------------------------- CLI Parser -----------------------------------------

data Command
  = Web {test :: Bool}
  | Generate

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "web" $
        Opt.info
          (Web <$> testFlag)
          (Opt.progDesc "Run the equifuzz TUI, connected to a remote host"),
      Opt.command "generate" $
        Opt.info
          (pure Generate)
          (Opt.progDesc "Generate a sample of a generator")
    ]
  where
    testFlag =
      Opt.switch
        ( Opt.long "test"
            <> Opt.help "Show test data on the interface, don't run any fuzzing"
        )

parseArgs :: IO Command
parseArgs =
  Opt.execParser $
    Opt.info
      (commandParser <**> Opt.helper)
      ( mconcat
          [ Opt.fullDesc,
            Opt.progDesc "Fuzzer for formal equivalence checkers"
          ]
      )

--------------------------- Testing --------------------------------------------

testThread :: (ExperimentProgress -> IO ()) -> IO ()
testThread reportProgress = void . forkIO . forever . try @SomeException $ do
  experiment <- mkTestExperiment
  reportProgress (Began experiment)

  threadDelay =<< getStdRandom (uniformR (5e6, 20e6))

  proofFound <-
    getStdRandom (uniformR (1 :: Int, 100)) <&> \x ->
      if
          | x < 10 -> Nothing
          | x < 20 -> Just True
          | otherwise -> Just False

  reportProgress
    ( Completed
        ExperimentResult
          { proofFound,
            runnerInfo = "Example data generator",
            counterExample = Just "counter example goes here",
            fullOutput = "blah\nblah\nblah",
            uuid = experiment.uuid
          }
    )
  return ()
  where
    mkTestExperiment :: IO Experiment
    mkTestExperiment = do
      uuid <- UUID.nextRandom
      return
        Experiment
          { uuid,
            designSpec =
              DesignSource
                { language = Verilog,
                  topName = "top",
                  source = "/* I would write some Verilog here, but it's late */"
                },
            designImpl =
              DesignSource
                { language = SystemC,
                  topName = "main",
                  source = "int main() { return 0; }"
                },
            expectedResult = False
          }
