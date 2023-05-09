{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

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

tuiMain :: SSHHost -> CommandPath -> IO ()
tuiMain host vcfPath = do
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
              (runVCFormal host vcfPath)
              (B.writeBChan eventChan . ExperimentProgress)
          )
          (const $ experimentThread eventChan)

genMain :: IO ()
genMain = do
  Experiment {designSpec, designImpl} <- mkSystemCConstantExperiment
  T.putStrLn designSpec.source
  putStrLn "---------"
  T.putStrLn designImpl.source

checkMain :: SSHHost -> CommandPath -> FilePath -> FilePath -> IO ()
checkMain host vcfPath path1 path2 = do
  uuid <- UUID.nextRandom
  designSpec <- designSourceFromFile path1
  designImpl <- designSourceFromFile path2
  -- We have to set expectedResult, but it doesn't actually matter
  let experiment = Experiment {expectedResult = True, uuid, designSpec, designImpl}

  printf "> Running VC Formal on %s and %s...\n" path1 path2
  result <- runVCFormal host vcfPath experiment

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
    Tui host vcfPath -> tuiMain host vcfPath
    Generate -> genMain
    Check host vcfPath path1 path2 -> checkMain host vcfPath path1 path2

--------------------------- CLI Parser -----------------------------------------

data Command
  = Tui SSHHost CommandPath
  | Generate
  | Check SSHHost CommandPath FilePath FilePath

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "tui" $
        Opt.info
          ( Tui
              <$> hostArg
              <*> vcfPathArg
          )
          (Opt.progDesc "Run the equifuzz TUI, connected to a remote host"),
      Opt.command "generate" $
        Opt.info
          (pure Generate)
          (Opt.progDesc "Generate a sample of a generator"),
      Opt.command "check" $
        Opt.info
          ( Check
              <$> hostArg
              <*> vcfPathArg
              <*> Opt.strArgument (Opt.metavar "FILE1")
              <*> Opt.strArgument (Opt.metavar "FILE2")
          )
          (Opt.progDesc "Run the equivalence checker on a pair of programs")
    ]
  where
    hostArg :: Opt.Parser SSHHost
    hostArg =
      Opt.strOption
        ( Opt.long "host"
            <> Opt.value "mp5617@ee-mill3.ee.ic.ac.uk"
            <> Opt.showDefault
            <> Opt.help "Host to run equivalence checker on"
            <> Opt.metavar "HOST"
        )

    vcfPathArg :: Opt.Parser SSHHost
    vcfPathArg =
      Opt.strOption
        ( Opt.long "vcf-path"
            <> Opt.value "/eda/synopsys/2022-23/RHELx86/VC-STATIC_2022.06-SP2/bin/vcf"
            <> Opt.showDefault
            <> Opt.help "Path to vcf on the remote host"
            <> Opt.metavar "PATH"
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
