{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import CLI (readFecType, runOptParse)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text.IO qualified as T
import Experiments (
  Experiment (..),
  genSystemCConstantExperiment,
  showEvaluation,
 )
import GenSystemC (GenConfig (..))
import Optics (view)
import Options.Applicative qualified as Opt
import ToolRestrictions (noMods)

data Args
  = Args
  { jsonMode :: Bool
  , genConfig :: GenConfig
  }

argParser :: Opt.Parser Args
argParser = do
  jsonMode <-
    Opt.switch . mconcat $
      [ Opt.long "json"
      , Opt.help "Output experiment in JSON format"
      ]

  genMods <-
    Opt.option (view #mods <$> readFecType) . mconcat $
      [ Opt.long "fec-type"
      , Opt.metavar "TYPE"
      , Opt.help "What FEC type to generate for (vcf|catapult|jasper)"
      , Opt.value noMods
      ]

  evaluations <-
    Opt.option Opt.auto . mconcat $
      [ Opt.long "evaluations"
      , Opt.metavar "COUNT"
      , Opt.help "How many values to evaluate the generated module at"
      , Opt.value 5
      , Opt.showDefault
      ]

  growSteps <-
    Opt.option Opt.auto . mconcat $
      [ Opt.long "grow-steps"
      , Opt.metavar "COUNT"
      , Opt.help "Size of the generated program"
      , Opt.value 30
      , Opt.showDefault
      ]

  return
    Args
      { genConfig =
          GenConfig
            { growSteps
            , genMods
            , evaluations
            }
      , ..
      }

main :: IO ()
main = do
  Args{genConfig, jsonMode} <- runOptParse "Generate a program with equifuzz" argParser

  experiment <- genSystemCConstantExperiment genConfig

  if jsonMode
    then LBS.putStrLn (encode experiment)
    else outputExperiment experiment

outputExperiment :: Experiment -> IO ()
outputExperiment Experiment{scSignature, scDesign, verilogDesign, knownEvaluations} = do
  T.putStrLn scDesign
  putStrLn "---------"
  T.putStrLn verilogDesign
  putStrLn "---------"
  mapM_
    (T.putStrLn . showEvaluation scSignature)
    knownEvaluations
