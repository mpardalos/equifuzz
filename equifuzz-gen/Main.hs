{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (replicateM_)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Experiments (
  Evaluation (..),
  Experiment (..),
  SignatureF (args),
  TextSignature,
  comparisonValueRaw,
  genSystemCConstantExperiment,
 )
import GenSystemC (GenConfig (..))
import Safe (readMay, readNote)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import ToolRestrictions (noMods)

genConfig :: GenConfig
genConfig =
  GenConfig
    { growSteps = 10
    , transformationAllowed = noMods
    , evaluations = 5
    }

main :: IO ()
main = do
  count <-
    getArgs >>= \case
      [readMay -> Just count] -> pure count
      [] -> pure 1
      _ -> die "Invalid arguments"

  replicateM_ count $ do
    Experiment{scSignature, scDesign, verilogDesign, knownEvaluations} <-
      genSystemCConstantExperiment genConfig
    T.putStrLn scDesign
    putStrLn "---------"
    T.putStrLn verilogDesign
    putStrLn "---------"
    mapM_
      (T.putStrLn . showEvaluation scSignature)
      knownEvaluations

showEvaluation :: TextSignature -> Evaluation -> Text
showEvaluation sig Evaluation{inputs, output} =
  case inputs of
    [] -> "* -> " <> comparisonValueRaw output
    (_ : _) ->
      "* "
        <> T.intercalate
          "\n  "
          ( [ name <> "=" <> comparisonValueRaw value
            | ((_, name), value) <- zip sig.args inputs
            ]
          )
        <> " -> \n  "
        <> comparisonValueRaw output
