{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}

module Main where

import Control.Applicative ((<**>))
import Control.Concurrent (forkFinally, forkIO, newMVar, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever, replicateM_, void)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID.V4 qualified as UUID
import Experiments
import Options.Applicative qualified as Opt
import System.Random (getStdRandom, uniformR)
import Text.Printf (printf)
import WebUI (handleProgress, newWebUIState, runWebUI)

experimentThread :: SSHHost -> CommandPath -> (ExperimentProgress -> IO ()) -> IO ()
experimentThread host vcfPath reportProgress =
  void $
    forkFinally
      ( experimentLoop
          mkSystemCConstantExperiment
          (runVCFormal host vcfPath)
          reportProgress
      )
      (const $ experimentThread host vcfPath reportProgress)

webMain :: Bool -> SSHHost -> CommandPath -> IO ()
webMain test host vcfPath = do
  stateVar <- newMVar newWebUIState
  replicateM_ 10 $
    if test
      then testThread (handleProgress stateVar)
      else experimentThread host vcfPath (handleProgress stateVar)

  runWebUI stateVar

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
    Web {test, sshHost, vcfPath} -> webMain test sshHost vcfPath
    Generate -> genMain
    Check host vcfPath path1 path2 -> checkMain host vcfPath path1 path2

--------------------------- CLI Parser -----------------------------------------

data Command
  = Web
      { test :: Bool,
        sshHost :: SSHHost,
        vcfPath :: CommandPath
      }
  | Generate
  | Check SSHHost CommandPath FilePath FilePath

commandParser :: Opt.Parser Command
commandParser =
  Opt.hsubparser . mconcat $
    [ Opt.command "web" $
        Opt.info
          ( Web
              <$> testFlag
              <*> hostArg
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
    testFlag =
      Opt.switch
        ( Opt.long "test"
            <> Opt.help "Show test data on the interface, don't run any fuzzing"
        )

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

--------------------------- Testing --------------------------------------------

testThread :: (ExperimentProgress -> IO ()) -> IO ()
testThread reportProgress = void . forkIO . forever . try @SomeException $ do
  experiment <- mkTestExperiment
  reportProgress (Began experiment)

  threadDelay 5e6

  proofFound <-
    getStdRandom (uniformR (0 :: Int, 2)) <&> \case
      0 -> Nothing
      1 -> Just True
      2 -> Just False
      _ -> error "Invalid value when generating random proofFound"

  reportProgress
    ( Completed
        ExperimentResult
          { proofFound,
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
