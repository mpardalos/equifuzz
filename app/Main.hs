{-# LANGUAGE LambdaCase #-}

module Main where

import Brick.BChan qualified as B
import Control.Applicative ((<**>))
import Control.Concurrent (forkFinally)
import Control.Monad (replicateM_, void)
import Data.Text.IO qualified as T
import Experiments
import Options.Applicative qualified as Opt
import TUI (AppEvent (..), runTUI)

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

main :: IO ()
main =
  parseArgs >>= \case
    Tui generator -> tuiMain generator
    Generate generator -> genMain generator

--------------------------- CLI Parser -----------------------------------------

type GeneratorName = String

type Generator = IO Experiment

data Command
  = Tui Generator
  | Generate Generator

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "tui" $
        Opt.info (Tui <$> generatorNameArg) (Opt.progDesc "Run the equifuzz TUI, connected to ee-mill3"),
      Opt.command "generate" $
        Opt.info (Generate <$> generatorNameArg) (Opt.progDesc "Generate a sample of a generator")
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
