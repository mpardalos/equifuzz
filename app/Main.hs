{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Brick.BChan qualified as B
import Control.Applicative ((<**>))
import Control.Concurrent (forkFinally)
import Control.Monad (replicateM_, void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID.V4 qualified as UUID
import Experiments
import Options.Applicative qualified as Opt
import TUI (AppEvent (..), runTUI)
import Text.Printf (printf)

tuiMain :: IO ()
tuiMain = do
  eventChan <- B.newBChan 20
  replicateM_ 10 $ experimentThread eventChan
  runTUI eventChan
  where
    experimentThread :: B.BChan AppEvent -> IO ()
    experimentThread eventChan =
      void $
        forkFinally
          ( experimentLoop
              mkSystemCConstantExperiment
              runVCFormal
              (B.writeBChan eventChan . ExperimentProgress)
          )
          (const $ experimentThread eventChan)

genMain :: IO ()
genMain = do
  Experiment {designSpec, designImpl} <- mkSystemCConstantExperiment
  T.putStrLn designSpec.source
  putStrLn "---------"
  T.putStrLn designImpl.source

checkMain :: FilePath -> FilePath -> IO ()
checkMain path1 path2 = do
  uuid <- UUID.nextRandom
  designSpec <- designSourceFromFile path1
  designImpl <- designSourceFromFile path2
  -- We have to set expectedResult, but it doesn't actually matter
  let experiment = Experiment {expectedResult = True, uuid, designSpec, designImpl}

  printf "> Running VC Formal on %s and %s...\n" path1 path2
  result <- runVCFormal experiment

  case result.proofFound of
    Just True -> putStrLn "✔️ It says they are equivalent"
    Just False -> putStrLn "❌ It says they are NOT equivalent"
    Nothing -> putStrLn "❔ It is inconclusive"

  case result.counterExample of
    Nothing -> pure ()
    Just txt -> do
      printf "╭─ Counter Example ───\n"
      putStrLn
        . T.unpack
        . ("│ " <>)
        . T.intercalate "\n│ "
        . T.lines
        $ txt
      printf "╰─────────────────────\n"

  let fullOutputFilename = "vcf.log"
  putStrLn ("Writing full output to " ++ fullOutputFilename)
  writeFile fullOutputFilename (T.unpack result.fullOutput)

main :: IO ()
main =
  parseArgs >>= \case
    Tui -> tuiMain
    Generate -> genMain
    Check path1 path2 -> checkMain path1 path2

--------------------------- CLI Parser -----------------------------------------

type GeneratorName = String

type Generator = IO Experiment

data Command
  = Tui
  | Generate
  | Check FilePath FilePath

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "tui" $
        Opt.info
          (pure Tui)
          (Opt.progDesc "Run the equifuzz TUI, connected to ee-mill3"),
      Opt.command "generate" $
        Opt.info
          (pure Generate)
          (Opt.progDesc "Generate a sample of a generator"),
      Opt.command "check" $
        Opt.info
          ( Check
              <$> Opt.strArgument (Opt.metavar "FILE1")
              <*> Opt.strArgument (Opt.metavar "FILE2")
          )
          (Opt.progDesc "Generate a sample of a generator")
    ]

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
