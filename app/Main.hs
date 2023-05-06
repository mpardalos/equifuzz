{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Brick.BChan qualified as B
import Control.Applicative ((<**>))
import Control.Concurrent (forkFinally)
import Control.Monad (replicateM_, void)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID.V4 qualified as UUID
import Experiments
import Options.Applicative qualified as Opt
import TUI (AppEvent (..), runTUI)
import Text.Printf (printf)

tuiMain :: IO Experiment -> IO ()
tuiMain generator = do
  eventChan <- B.newBChan 20
  replicateM_ 10 $ experimentThread eventChan
  runTUI eventChan
  where
    experimentThread :: B.BChan AppEvent -> IO ()
    experimentThread eventChan =
      void $
        forkFinally
          (experimentLoop generator runVCFormal (B.writeBChan eventChan . ExperimentProgress))
          (const $ experimentThread eventChan)

genMain :: IO Experiment -> IO ()
genMain generator = do
  Experiment {design1, design2} <- generator
  T.putStrLn design1.source
  putStrLn "---------"
  T.putStrLn design2.source

checkMain :: FilePath -> FilePath -> IO ()
checkMain path1 path2 = do
  uuid <- UUID.nextRandom
  design1 <- designSourceFromFile path1
  design2 <- designSourceFromFile path2
  -- We have to set expectedResult, but it doesn't actually matter
  let experiment = Experiment {expectedResult = True, uuid, design1, design2}

  printf "Running VC Formal on %s and %s ...\n" path1 path2
  result <- runVCFormal experiment

  case result.proofFound of
    Just True -> putStrLn "  It says they are equivalent"
    Just False -> putStrLn "  It says they are NOT equivalent"
    Nothing -> putStrLn "  It is inconclusive"
  let fullOutputFilename = "vcf.log"
  putStrLn ("Writing full output to " ++ fullOutputFilename)
  writeFile fullOutputFilename (T.unpack result.fullOutput)

main :: IO ()
main =
  parseArgs >>= \case
    Tui generator -> tuiMain generator
    Generate generator -> genMain generator
    Check path1 path2 -> checkMain path1 path2

--------------------------- CLI Parser -----------------------------------------

type GeneratorName = String

type Generator = IO Experiment

data Command
  = Tui Generator
  | Generate Generator
  | Check FilePath FilePath

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "tui" $
        Opt.info
          (Tui <$> generatorNameArg)
          (Opt.progDesc "Run the equifuzz TUI, connected to ee-mill3"),
      Opt.command "generate" $
        Opt.info
          (Generate <$> generatorNameArg)
          (Opt.progDesc "Generate a sample of a generator"),
      Opt.command "check" $
        Opt.info
          ( Check
              <$> Opt.strArgument (Opt.metavar "FILE1")
              <*> Opt.strArgument (Opt.metavar "FILE2")
          )
          (Opt.progDesc "Generate a sample of a generator")
    ]
  where
    generatorNameArg =
      Opt.option
        ( Opt.maybeReader $ \case
            "systemc-constant" -> Just mkSystemCConstantExperiment
            "systemc-verilog" -> Just mkSystemCVerilogExperiment
            _ -> Nothing
        )
        ( mconcat
            [ Opt.long "generator",
              Opt.short 'g',
              Opt.metavar "GEN-NAME",
              Opt.help "The experiment generator to use. One of: systemc-constant (default), systemc-verilog",
              Opt.value mkSystemCConstantExperiment
            ]
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
