{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Experiments (
  Experiment (..),
  genSystemCConstantExperiment,
  generateProcessToExperiment,
  saveExperimentWithResult,
  reportExperiment,
  ExperimentId (..),
  newExperimentId,
  ExperimentSequenceId (..),
  newExperimentSequenceId,
  ComparisonValue (..),
  comparisonValueRaw,
  mkComparisonValueWithWidth,
  DesignSource (..),
  ExperimentResult (..),
  Evaluation (..),
  TextSignature,
  SC.SignatureF (..),
  showEvaluation,
  RunSystemCProgramResult (..),
  runSystemCProgram,
) where

import Control.Monad (forM, replicateM, when)
import Control.Monad.Random (MonadRandom (getRandom), evalRand)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (intToDigit)
import Data.Data (Data)
import Data.Functor
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import GenSystemC (GenConfig (..), GenerateProcess (..), genSystemCProcess, generateProcessToSystemC)
import Numeric (showIntAtBase)
import Optics
import Prettyprinter (Pretty (..), (<+>))
import Safe (foldr1May, readNote)
import Shelly ((</>))
import System.Environment.Blank (getEnvDefault)
import System.Process (readProcessWithExitCode)
import SystemC qualified as SC
import ToolRestrictions ()
import Util (chunksOf, mkdir_p, runBash, whenJust)

-- | Identifies a sequence of experiments
newtype ExperimentSequenceId = ExperimentSequenceId {uuid :: UUID}
  deriving (Show, Eq, Ord, Generic, Data)
  deriving newtype (FromJSON, ToJSON)

newExperimentSequenceId :: IO ExperimentSequenceId
newExperimentSequenceId = ExperimentSequenceId <$> UUID.nextRandom

-- | Identifies a single experiment within a sequence of experiments
newtype ExperimentId = ExperimentId {uuid :: UUID}
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving newtype (FromJSON, ToJSON)

newExperimentId :: IO ExperimentId
newExperimentId = ExperimentId <$> UUID.nextRandom

-- | literal value represented as (big-endian) string of 0s and 1s
newtype ComparisonValue = UnsafeComparisonValue Text
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

mkComparisonValueWord ::
  -- | Width
  Int ->
  -- | Value
  Word ->
  ComparisonValue
mkComparisonValueWord w v = mkComparisonValueWithWidth w (T.pack (showIntAtBase 2 intToDigit v ""))

mkComparisonValue :: Text -> ComparisonValue
mkComparisonValue t
  | Just c <- T.find (`notElem` ("01" :: [Char])) t =
      error ("Unexpected character in comparison value: " ++ show c)
  | otherwise = UnsafeComparisonValue t

mkComparisonValueWithWidth :: Int -> Text -> ComparisonValue
mkComparisonValueWithWidth w t
  | T.length t < w = mkComparisonValue (T.replicate (w - T.length t) "0" <> t)
  | otherwise = mkComparisonValue (T.takeEnd w t)

comparisonValueRaw :: ComparisonValue -> Text
comparisonValueRaw (UnsafeComparisonValue t) = t

comparisonValueAsVerilog :: ComparisonValue -> Text
comparisonValueAsVerilog (UnsafeComparisonValue t) =
  T.pack (show (T.length t)) <> "'b" <> t

data Evaluation = Evaluation
  { inputs :: [ComparisonValue]
  , output :: ComparisonValue
  }
  deriving (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty ComparisonValue where
  pretty = pretty . comparisonValueAsVerilog

instance Pretty Evaluation where
  pretty Evaluation{inputs, output} =
    pretty inputs <+> " -> " <+> pretty output

type TextSignature = SC.SignatureF Text

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

data Experiment = Experiment
  { experimentId :: ExperimentId
  , expectedResult :: Bool
  -- ^ True if we expect the modules to be equivalent, False if we expect them not to be
  , scSignature :: TextSignature
  , scDesign :: Text
  , verilogDesign :: Text
  , size :: Int
  , generateProcess :: GenerateProcess
  , knownEvaluations :: [Evaluation]
  -- ^ Input vectors and matching results at which the design will be evaluated
  , extraInfos :: Map Text Text
  -- ^ Extra information to be displayed along with the experiment
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data DesignSource = DesignSource
  { topName :: Text
  , source :: Text
  }
  deriving (Show, Eq, Ord)

data ExperimentResult = ExperimentResult
  { experimentId :: ExperimentId
  , proofFound :: Maybe Bool
  , counterExample :: Maybe Text
  , fullOutput :: Text
  , extraInfos :: Map Text Text
  }
  deriving (Show, Generic, Eq)

-- | Make an experiment using the SystemC-constant generator. Needs to have
-- icarus verilog (`iverilog`) available locally
genSystemCConstantExperiment :: GenConfig -> IO Experiment
genSystemCConstantExperiment cfg =
  generateProcessToExperiment =<< genSystemCProcess cfg

-- | Modify a function so that it returns zero on inputs not in the provided
-- list of evaluations.  For inputs within the list of evaluations, it runs the
-- same code as the original function.
limitToEvaluations :: [Evaluation] -> SC.FunctionDeclaration -> SC.FunctionDeclaration
limitToEvaluations evals decl =
  let
    goodInputs = map (view #inputs) evals
    goodInputsLabelled = map (zip decl.sig.args) goodInputs
    inputChecks = map (map (\((t, name), value) -> SC.BinOp SC.CBool (SC.Variable t name) SC.Equals (SC.Literal t (comparisonValueAsSC t value)))) goodInputsLabelled
    evaluationConditions =
      map
        ( fromMaybe (SC.Constant SC.CBool 1)
            . foldr1May (\l r -> SC.BinOp SC.CBool l SC.LogicalAnd r)
        )
        inputChecks
    inputValidCondition =
      fromMaybe (SC.Constant SC.CBool 0)
        . foldr1May (\l r -> SC.BinOp SC.CBool l SC.LogicalOr r)
        $ evaluationConditions
    earlyReturn :: SC.Statement =
      SC.If
        (SC.UnaryOp SC.CBool SC.LogicalNot inputValidCondition)
        (SC.Return (defaultValueSC decl.sig.returnType))
        Nothing
   in
    decl{SC.body = earlyReturn : decl.body}

generateProcessToExperiment :: GenerateProcess -> IO Experiment
generateProcessToExperiment generateProcess@GenerateProcess{seed, inputValuesSeed, transformations, cfg} = do
  let rawDesign = generateProcessToSystemC cfg "dut" generateProcess

  -- We use the value given from generate process so that we get exactly the
  -- same experiment for reductions of the same process
  let inputss
        | null rawDesign.sig.args = [[]]
        | otherwise =
            evalRand
              (replicateM cfg.evaluations $ mapM (comparisonValueOfType . fst) rawDesign.sig.args)
              inputValuesSeed

  simulationResult <- simulateSystemCAt rawDesign inputss

  let knownEvaluations = [Evaluation{..} | (inputs, output) <- zip inputss simulationResult.results]

  let scDesign = limitToEvaluations knownEvaluations rawDesign

  let verilogDesign = verilogImplForEvals scDesign knownEvaluations

  let extraInfos =
        simulationResult.extraInfos
          <> [
               ( "Transformations"
               , T.unlines
                  ( T.pack ("Seed: " ++ show seed)
                      : [ T.pack (show n) <> " " <> T.pack (show t)
                        | (n, t) <- zip [0 :: Int ..] transformations
                        ]
                  )
               )
             ]

  experimentId <- newExperimentId
  return
    Experiment
      { experimentId
      , -- Expect a negative result for programs with UB
        expectedResult = not simulationResult.hasUndefinedBehaviour
      , scSignature = fmap SC.genSource scDesign.sig
      , scDesign = SC.genSource scDesign
      , generateProcess
      , verilogDesign
      , size = length transformations
      , extraInfos
      , knownEvaluations
      }

comparisonValueOfType :: MonadRandom m => SC.SCType -> m ComparisonValue
comparisonValueOfType t = mkComparisonValueWord (typeWidth t) <$> getRandom

data SystemCSimulationResult = SystemCSimulationResult
  { results :: [ComparisonValue]
  , hasUndefinedBehaviour :: Bool
  , extraInfos :: Map Text Text
  }
  deriving (Show, Generic)

comparisonValueAsSC :: SC.SCType -> ComparisonValue -> Text
comparisonValueAsSC t val =
  case t of
    SC.SCFixed{i} -> fxLiteralWithCast i
    SC.SCUFixed{i} -> fxLiteralWithCast i
    SC.CDouble -> literalWithMemCpy
    _ -> literalWithCast
 where
  fxLiteralWithCast i =
    let
      rawLiteral = comparisonValueRaw val
      fixedPointLiteral = T.take i rawLiteral <> "." <> T.drop i rawLiteral
     in
      SC.genSource t <> "(\"0b" <> fixedPointLiteral <> "\")"
  literalWithMemCpy = "bit_cast<" <> SC.genSource t <> ">(" <> literal <> ")"
  literal = "0b" <> comparisonValueRaw val
  literalWithCast = SC.genSource t <> "(" <> literal <> ")"

-- | Run the SystemC function and return its output represented as text of a
-- Verilog-style constant. We use this output format because the normal
-- (decimal) output makes the representation of the output non-obvious. E.g. -1
-- as an sc_uint<8> or a sc_fixed<10,3> are represented completely differently.
-- With the default SystemC output, we would get "-1" in both cases, but here we
-- (correctly) get "8'b11111111" and "10'b1110000000"
simulateSystemCAt :: SC.FunctionDeclaration -> [[ComparisonValue]] -> IO SystemCSimulationResult
simulateSystemCAt decl@SC.FunctionDeclaration{sig = SC.Signature{returnType, name, args}} inputss = do
  let simulateAtValues values =
        let
          call =
            name
              <> "("
              <> T.intercalate ", " [comparisonValueAsSC t val | ((t, _), val) <- zip args values]
              <> ")"

          widthExpr :: String = case returnType of
            SC.SCInt n -> show n
            SC.SCUInt n -> show n
            SC.SCBigInt n -> show n
            SC.SCBigUInt n -> show n
            SC.SCFixed w _ -> show w
            SC.SCUFixed w _ -> show w
            SC.SCLogic -> "1"
            SC.SCBV n -> show n
            SC.SCLV n -> show n
            SC.SCFxnumBitref -> "1"
            SC.SCFxnumSubref{} -> [i|#{call}.length()|]
            SC.SCIntSubref{} -> [i|#{call}.length()|]
            SC.SCUIntSubref{} -> [i|#{call}.length()|]
            SC.SCSignedSubref{} -> [i|#{call}.length()|]
            SC.SCUnsignedSubref{} -> [i|#{call}.length()|]
            SC.SCIntBitref -> "1"
            SC.SCUIntBitref -> "1"
            SC.SCSignedBitref -> "1"
            SC.SCUnsignedBitref -> "1"
            SC.CUInt -> "32"
            SC.CInt -> "32"
            SC.CDouble -> "64"
            SC.CBool -> "1"

          scToString :: Text = [i|std::cout << #{call}.to_string(sc_dt::SC_BIN, false) << std::endl;|]
          scPrintToString :: Text = [i|#{call}.print(); std::cout << std::endl;|]
          boolToString :: Text = [i|std::cout << (#{call} ? "1" : "0") << std::endl;|]
          bitsetToString :: Text = [i|std::cout << std::bitset<32>(#{call}) << std::endl;|]
          doubleToString :: Text =
            [__i|
            auto value = #{call};
            std::cout << std::bitset<64>(*reinterpret_cast<unsigned long*>(&value)) << std::endl;
            |]

          showValue :: Text = case returnType of
            SC.SCInt{} -> scToString
            SC.SCUInt{} -> scToString
            SC.SCBigInt{} -> scToString
            SC.SCBigUInt{} -> scToString
            SC.SCFixed{} -> scToString
            SC.SCUFixed{} -> scToString
            SC.SCFxnumSubref{} -> scToString
            SC.SCIntSubref{} -> scToString
            SC.SCUIntSubref{} -> scToString
            SC.SCSignedSubref{} -> scToString
            SC.SCUnsignedSubref{} -> scToString
            SC.SCFxnumBitref{} -> boolToString
            SC.SCIntBitref -> boolToString
            SC.SCUIntBitref -> boolToString
            SC.SCSignedBitref -> boolToString
            SC.SCUnsignedBitref -> boolToString
            SC.SCLogic -> scPrintToString
            SC.SCBV{} -> scToString
            SC.SCLV{} -> scToString
            SC.CUInt -> bitsetToString
            SC.CInt -> bitsetToString
            SC.CDouble -> doubleToString
            SC.CBool -> boolToString
         in
          [__i|
              {
                std::cout << #{widthExpr} << std::endl;
                #{showValue}
                std::cout << "---" << std::endl;
              }
              |]

  let evaluations = T.unlines (map simulateAtValues inputss)

  let fullSource =
        [__i|
            #{SC.includeHeader}
            \#include <iostream>
            \#include <bitset>
            \#include <cstring>
            \#include <cstdint>

            template<typename T, typename U>
            constexpr T bit_cast(U&& u) {
                static_assert(sizeof(T) == sizeof(U), "Types must have same size");
                T result;
                std::memcpy(&result, &u, sizeof(T));
                return result;
            }

            #{SC.genSource decl}

            int sc_main(int argc, char **argv) {
                #{evaluations}
                return 0;
            }
            |]

  RunSystemCProgramResult{..} <- runSystemCProgram fullSource

  let hasUndefinedBehaviour = "undefined-behavior" `T.isInfixOf` programStdErr

  results <- forM (chunksOf 3 (T.lines programStdOut)) $ \segment -> do
    let (reportedWidth, reportedValue) =
          case segment of
            [line1, line2, _separator] -> (line1, line2)
            _ -> error ("Unexpected output line count from SystemC program: " ++ show (length (T.lines programStdOut)))

    let width = readNote "Parse width of SystemC value" . T.unpack $ reportedWidth
    let value = T.replace "." "" reportedValue

    return (mkComparisonValueWithWidth width value)

  return
    SystemCSimulationResult
      { results
      , hasUndefinedBehaviour
      , extraInfos =
          [ ("Clang exit code", T.pack . show $ clangExitCode)
          , ("Clang stdout", clangStdOut)
          , ("Clang stderr", clangStdErr)
          , ("Program exit code", T.pack . show $ programExitCode)
          , ("Program stdout", programStdOut)
          , ("Program stderr", programStdErr)
          , -- TODO: Removed to extract runSystemCProgram below, try to add them back in
            -- , ("SystemC Home", T.pack systemcHome)
            -- , ("Clang Path", T.pack clangPath)
            ("Simulation Source", fullSource)
          ]
      }

data RunSystemCProgramResult = RunSystemCProgramResult
  { clangExitCode :: ExitCode
  , clangStdOut :: Text
  , clangStdErr :: Text
  , programExitCode :: ExitCode
  , programStdOut :: Text
  , programStdErr :: Text
  }

runSystemCProgram :: Text -> IO RunSystemCProgramResult
runSystemCProgram fullSource = do
  tmpDir <- T.strip <$> runBash "mktemp -d"
  let cppPath = tmpDir <> "/main.cpp"
  let binPath = tmpDir <> "/main"

  TIO.writeFile (T.unpack cppPath) fullSource
  systemcHome <- getEnvDefault "SYSTEMC_HOME" "/usr"
  let systemcIncludePath = systemcHome <> "/include"
  let systemcLibraryPath = systemcHome <> "/lib"
  clangPath <- getEnvDefault "EQUIFUZZ_CLANG" "clang++"
  (clangExitCode, clangStdOut, clangStdErr) <-
    readProcessWithExitCode clangPath ["-fsanitize=undefined", "-I", systemcIncludePath, "-L", systemcLibraryPath, "-lsystemc", T.unpack cppPath, "-o", T.unpack binPath] ""

  when (clangExitCode /= ExitSuccess) $ do
    error ("Failed compiling experiment. Clang output:\n" ++ clangStdOut ++ "\n" ++ clangStdErr ++ "\n-----\nProgram:\n" ++ T.unpack fullSource ++ "\n---\n")

  (programExitCode, programStdOut, programStdErr) <- readProcessWithExitCode (T.unpack binPath) [] ""
  void $ runBash ("rm -r " <> tmpDir)

  return
    RunSystemCProgramResult
      { clangExitCode
      , clangStdOut = T.pack clangStdOut
      , clangStdErr = T.pack clangStdErr
      , programExitCode
      , programStdOut = T.pack programStdOut
      , programStdErr = T.pack programStdErr
      }

verilogImplForEvals :: SC.FunctionDeclaration -> [Evaluation] -> Text
verilogImplForEvals scFun evals =
  [__i|
      module top(#{decls});
      #{body}
      endmodule
      |]
 where
  decls = T.intercalate ", " (inputDecls ++ [outputDecl])

  body :: Text
  body
    | not (null inputDecls) =
        [__i|
        always_comb begin
          out = #{defaultValueVerilog (scFun ^. #sig % #returnType)};
          case (#{concatInputs})
            #{cases}
          endcase
        end
          |]
    | (evalHead : _) <- evals =
        [i|assign out = #{comparisonValueAsVerilog (evalHead ^. #output)};|]
    | otherwise =
        [i|assign out = {#{outputWidth}{X}};|]

  inputDecls :: [Text] =
    [ [i|input wire [#{typeWidth t - 1}:0] #{name}|]
    | (t, name) <- scFun.sig.args
    ]

  outputWidth = typeWidth scFun.sig.returnType
  outputDecl :: Text = [i|output reg [#{outputWidth - 1}:0] out|]

  concatInputs :: Text =
    "{" <> T.intercalate ", " [name | (_, name) <- scFun.sig.args] <> "}"

  cases :: Text =
    T.intercalate
      "\n    "
      [ let concatInputVals = T.intercalate ", " (map comparisonValueAsVerilog inputs)
         in [i|{#{concatInputVals}}: out = #{comparisonValueAsVerilog output};|]
      | Evaluation{inputs, output} <- evals
      ]

defaultValueVerilog :: SC.SCType -> Text
defaultValueVerilog _ = "0" -- Don't worry about it. Works for any type/width

defaultValueSC :: SC.SCType -> SC.Expr
defaultValueSC SC.SCLogic = SC.Literal SC.SCLogic "sc_dt::sc_logic(0)"
defaultValueSC t = SC.Literal t "0"

-- TODO: Make this configurable. Differs per EC.

typeWidth :: SC.SCType -> Int
typeWidth (SC.SCInt n) = n
typeWidth (SC.SCUInt n) = n
typeWidth (SC.SCBigInt n) = n
typeWidth (SC.SCBigUInt n) = n
typeWidth SC.SCFixed{w} = w
typeWidth SC.SCUFixed{w} = w
typeWidth SC.SCFxnumSubref{width} = width
typeWidth SC.SCIntSubref{width} = width
typeWidth SC.SCUIntSubref{width} = width
typeWidth SC.SCSignedSubref{width} = width
typeWidth SC.SCUnsignedSubref{width} = width
typeWidth SC.SCFxnumBitref = 1
typeWidth SC.SCIntBitref = 1
typeWidth SC.SCUIntBitref = 1
typeWidth SC.SCSignedBitref = 1
typeWidth SC.SCUnsignedBitref = 1
typeWidth SC.SCLogic = 1
typeWidth SC.SCBV{width} = width
typeWidth SC.SCLV{width} = width
typeWidth SC.CUInt = 32
typeWidth SC.CInt = 32
typeWidth SC.CDouble = 64
typeWidth SC.CBool = 1

reportExperiment :: Experiment -> ExperimentResult -> IO ()
reportExperiment experiment result = do
  TIO.putStrLn "--- spec.cpp --------------------------------"
  TIO.putStrLn experiment.scDesign
  TIO.putStrLn "---"
  mapM_
    (TIO.putStrLn . showEvaluation experiment.scSignature)
    experiment.knownEvaluations

  TIO.putStrLn "--- impl.sv --------------------------------"
  TIO.putStrLn experiment.verilogDesign

  -- TIO.putStrLn "--- Output --------------------------------"
  -- TIO.putStrLn result.fullOutput
  TIO.writeFile "log.txt" result.fullOutput

  whenJust result.counterExample $ \cex -> do
    TIO.putStrLn "--- Counter-Example -----------------------"
    TIO.putStrLn cex

  TIO.putStrLn "-------------------------------------------"
  TIO.putStr "Result: "
  TIO.putStrLn $
    case result.proofFound of
      Just True -> "Equivalent"
      Just False -> "Non-equivalent"
      Nothing -> "Inconclusive"

-- | Save information about the experiment to the experiments/ directory
saveExperimentWithResult :: FilePath -> Experiment -> ExperimentResult -> IO ()
saveExperimentWithResult dir experiment result = do
  -- let localExperimentDir = "experiments/" <> UUID.toText experiment.experimentId.uuid

  mkdir_p (T.pack dir)
  TIO.writeFile (dir </> ("spec.cpp" :: Text)) experiment.scDesign
  TIO.writeFile (dir </> ("impl.sv" :: Text)) experiment.verilogDesign

  TIO.writeFile
    (dir </> ("full_output.txt" :: Text))
    result.fullOutput

  whenJust result.counterExample $
    TIO.writeFile
      (dir </> ("counter_example.txt" :: Text))

  TIO.writeFile
    (dir </> ("info.txt" :: Text))
    [__i|
        Proof Found     : #{result ^. #proofFound}
        Counter Example : #{isJust (result ^. #counterExample)}
        |]

makeFieldLabelsNoPrefix ''SystemCSimulationResult
makeFieldLabelsNoPrefix ''Experiment
makeFieldLabelsNoPrefix ''DesignSource
makeFieldLabelsNoPrefix ''ExperimentResult
makeFieldLabelsNoPrefix ''ExperimentId
makeFieldLabelsNoPrefix ''ExperimentSequenceId
