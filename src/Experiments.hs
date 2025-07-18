{-# LANGUAGE DataKinds #-}
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

{-# HLINT ignore "Use <$>" #-}

module Experiments (
  Experiment(..),
  mkSystemCConstantExperiment,
  generateProcessToExperiment,
  saveExperiment,
  ExperimentId(..),
  newExperimentId,
  ExperimentSequenceId(..),
  newExperimentSequenceId,
  ComparisonValue (..),
  comparisonValueRaw,
  DesignSource (..),
  ExperimentResult (..),
  ExperimentProgress (..),
  Evaluation(..),
) where

import Control.Monad (replicateM)
import Control.Monad.Random.Strict (MonadRandom (getRandom), evalRandIO)
import Data.Char (intToDigit)
import Data.Data (Data)
import Data.Either (fromRight)
import Data.Functor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import GenSystemC (GenConfig (..), GenerateProcess (..), genSystemC, generateFromProcess)
import Numeric (showIntAtBase)
import Optics
import Prettyprinter (Pretty (..), (<+>))
import Safe (foldr1May)
import Shelly ((</>))
import System.Environment.Blank (getEnvDefault)
import System.Process (readProcessWithExitCode)
import SystemC qualified as SC
import Util (mkdir_p, runBash, whenJust)

-- | Identifies a sequence of experiments
newtype ExperimentSequenceId = ExperimentSequenceId {uuid :: UUID}
  deriving (Show, Eq, Ord, Generic, Data)

newExperimentSequenceId :: IO ExperimentSequenceId
newExperimentSequenceId = ExperimentSequenceId <$> UUID.nextRandom

-- | Identifies a single experiment within a sequence of experiments
newtype ExperimentId = ExperimentId {uuid :: UUID}
  deriving (Show, Eq, Ord, Generic, Data)

newExperimentId :: IO ExperimentId
newExperimentId = ExperimentId <$> UUID.nextRandom

-- | literal value represented as (big-endian) string of 0s and 1s
newtype ComparisonValue = UnsafeComparisonValue Text
  deriving newtype (Eq, Ord, Show)

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

instance Pretty ComparisonValue where
  pretty = pretty . comparisonValueAsVerilog

instance Pretty Evaluation where
  pretty Evaluation{inputs, output} =
    pretty inputs <+> " -> " <+> pretty output

data Experiment = Experiment
  { experimentId :: ExperimentId
  , expectedResult :: Bool
  -- ^ True if we expect the modules to be equivalent, False if we expect them not to be
  , scDesign :: SC.FunctionDeclaration
  , verilogDesign :: Text
  , size :: Int
  , generateProcess :: GenerateProcess
  -- ^ Used for ordering reductions of the same experiment. Will probably be
  -- the number of transformations used to generate it
  , knownEvaluations :: [Evaluation]
  -- ^ Input vectors and matching results at which the design will be evaluated
  , extraInfos :: Map Text Text
  -- ^ Extra information to be displayed along with the experiment
  }
  deriving (Generic, Show)

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

data ExperimentProgress
  = ExperimentStarted ExperimentSequenceId Experiment
  | ExperimentCompleted ExperimentSequenceId ExperimentResult
  | ExperimentSequenceCompleted ExperimentSequenceId
  deriving (Show)

-- | Make an experiment using the SystemC-constant generator. Needs to have
-- icarus verilog (`iverilog`) available locally
mkSystemCConstantExperiment :: GenConfig -> IO Experiment
mkSystemCConstantExperiment cfg =
  generateProcessToExperiment cfg =<< genSystemC cfg

-- | Modify a function so that it returns zero on inputs not in the provided
-- list of evaluations.  For inputs within the list of evaluations, it runs the
-- same code as the original function.
limitToEvaluations :: [Evaluation] -> SC.FunctionDeclaration -> SC.FunctionDeclaration
limitToEvaluations evals decl =
  let
    goodInputs = map (view #inputs) evals
    goodInputsLabelled = map (zip decl.args) goodInputs
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
        (SC.Return (defaultValueSC decl.returnType))
        Nothing
   in
    decl{SC.body = earlyReturn : decl.body}

generateProcessToExperiment :: GenConfig -> GenerateProcess -> IO Experiment
generateProcessToExperiment cfg generateProcess@GenerateProcess{seed, transformations} = do
  let rawDesign = generateFromProcess cfg "dut" generateProcess

  inputss <-
    replicateM cfg.evaluations $
      mapM (comparisonValueOfType . fst) rawDesign.args

  simulationResults <- mapM (simulateSystemCAt rawDesign) inputss

  let knownEvaluations = [Evaluation{..} | (inputs, output) <- zip inputss (view #result <$> simulationResults)]
  let hasUB = orOf (each % #hasUndefinedBehaviour) simulationResults
  -- let extraInfo = T.intercalate "-\n" extraInfos

  let scDesign = limitToEvaluations knownEvaluations rawDesign

  let verilogDesign = verilogImplForEvals scDesign knownEvaluations

  let evaluationInfos =
        Map.unions
          [ let label = "Evaluation " <> T.pack (show idx) <> " - "
             in Map.mapKeys (label <>) extraInfos
          | (idx, SystemCSimulationResult{extraInfos}) <- zip [1 :: Int ..] simulationResults
          ]

  let extraInfos =
        [
          ( "Transformations"
          , T.unlines
              ( T.pack ("Seed: " ++ show seed)
                  : [ T.pack (show n) <> " " <> T.pack (show t)
                    | (n, t) <- zip [0 :: Int ..] transformations
                    ]
              )
          )
        ]
          <> evaluationInfos

  experimentId <- newExperimentId
  return
    Experiment
      { experimentId
      , expectedResult = not hasUB -- Expect a negative result for programs with UB
      , scDesign
      , generateProcess
      , verilogDesign
      , size = length transformations
      , extraInfos
      , knownEvaluations
      }

comparisonValueOfType :: MonadRandom m => SC.SCType -> m ComparisonValue
comparisonValueOfType t = case SC.knownWidth t of
  Nothing -> mkComparisonValueWord 32 <$> getRandom
  Just w -> mkComparisonValueWord w <$> getRandom

data SystemCSimulationResult = SystemCSimulationResult
  { result :: ComparisonValue
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
simulateSystemCAt :: SC.FunctionDeclaration -> [ComparisonValue] -> IO SystemCSimulationResult
simulateSystemCAt decl@SC.FunctionDeclaration{returnType, name, args} inputs = do
  let callArgs =
        T.intercalate
          ", "
          [ comparisonValueAsSC t val
          | ((t, _), val) <- zip args inputs
          ]
  let call = name <> "(" <> callArgs <> ")"

  let widthExprOrWidth :: Either Text Int = case returnType of
        SC.SCInt n -> Right n
        SC.SCUInt n -> Right n
        SC.SCBigInt n -> Right n
        SC.SCBigUInt n -> Right n
        SC.SCFixed w _ -> Right w
        SC.SCUFixed w _ -> Right w
        SC.SCLogic -> Right 1
        SC.SCBV n -> Right n
        SC.SCLV n -> Right n
        SC.SCFxnumSubref{} -> Left [i|#{call}.length()|]
        SC.SCIntSubref{} -> Left [i|#{call}.length()|]
        SC.SCUIntSubref{} -> Left [i|#{call}.length()|]
        SC.SCSignedSubref{} -> Left [i|#{call}.length()|]
        SC.SCUnsignedSubref{} -> Left [i|#{call}.length()|]
        SC.SCIntBitref -> Right 1
        SC.SCUIntBitref -> Right 1
        SC.SCSignedBitref -> Right 1
        SC.SCUnsignedBitref -> Right 1
        SC.CUInt -> Right 32
        SC.CInt -> Right 32
        SC.CDouble -> Right 64
        SC.CBool -> Right 1

  let showWidth :: Text = case widthExprOrWidth of
        Left e -> [i|std::cout << #{e} << std::endl;|]
        Right _ -> "std::cout << 0 << std::endl;"

  let scToString :: Text = [i|std::cout << #{call}.to_string(sc_dt::SC_BIN, false) << std::endl;|]
  let scPrintToString :: Text = [i|#{call}.print(); std::cout << std::endl;|]
  let boolToString :: Text = [i| std::cout << (#{call} ? "1" : "0") << std::endl;|]
  let bitsetToString :: Text = [i| std::cout << std::bitset<32>(#{call}) << std::endl;|]
  let doubleToString :: Text =
        [i|
         auto value = #{call};
         std::cout << std::bitset<64>(*reinterpret_cast<unsigned long*>(&value)) << std::endl;
         |]

  let showValue :: Text = case returnType of
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
                #{showWidth}
                #{showValue}
                return 0;
            }
            |]

  tmpDir <- T.strip <$> runBash "mktemp -d"
  let cppPath = tmpDir <> "/main.cpp"
  let binPath = tmpDir <> "/main"

  writeFile (T.unpack cppPath) fullSource
  systemcHome <- getEnvDefault "SYSTEMC_HOME" "/usr"
  let systemcIncludePath = systemcHome <> "/include"
  let systemcLibraryPath = systemcHome <> "/lib"
  clangPath <- getEnvDefault "EQUIFUZZ_CLANG" "clang++"
  (clangExitCode, clangStdOut, clangStdErr) <-
    readProcessWithExitCode clangPath ["-fsanitize=undefined", "-I", systemcIncludePath, "-L", systemcLibraryPath, "-lsystemc", T.unpack cppPath, "-o", T.unpack binPath] ""

  (programExitCode, T.pack -> programOut, T.pack -> programStderr) <-
    readProcessWithExitCode (T.unpack binPath) [] ""
  let hasUndefinedBehaviour = "undefined-behavior" `T.isInfixOf` programStderr
  void $ runBash ("rm -r " <> tmpDir)

  let (reportedWidth, reportedValue) =
        case T.lines programOut of
          [line1, line2] -> (line1, line2)
          ls -> error ("Unexpected output line count from SystemC program: " ++ show (length ls))

  let width = fromRight (read . T.unpack $ reportedWidth) widthExprOrWidth
  let value = T.replace "." "" reportedValue

  return
    SystemCSimulationResult
      { result = mkComparisonValueWithWidth width value
      , hasUndefinedBehaviour
      , extraInfos =
          [ ("Clang exit code", T.pack . show $ clangExitCode)
          , ("Clang stdout", T.pack clangStdOut)
          , ("Clang stderr", T.pack clangStdErr)
          , ("Program exit code", T.pack . show $ programExitCode)
          , ("Program stdout", programOut)
          , ("Program stderr", programStderr)
          , ("SystemC Home", T.pack systemcHome)
          , ("Clang Path", T.pack clangPath)
          , ("Simulation Source", T.pack fullSource)
          ]
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
          out = #{defaultValueVerilog (scFun ^. #returnType)};
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
    | (t, name) <- scFun.args
    ]

  outputWidth = typeWidth scFun.returnType
  outputDecl :: Text = [i|output reg [#{outputWidth - 1}:0] out|]

  concatInputs :: Text =
    "{" <> T.intercalate ", " [name | (_, name) <- scFun.args] <> "}"

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

-- | Save information about the experiment to the experiments/ directory
saveExperiment :: Experiment -> ExperimentResult -> IO ()
saveExperiment experiment result = do
  let localExperimentDir = "experiments/" <> UUID.toText experiment.experimentId.uuid

  mkdir_p localExperimentDir
  TIO.writeFile (localExperimentDir </> ("spec.cpp" :: Text)) (SC.genSource experiment.scDesign)
  TIO.writeFile (localExperimentDir </> ("impl.sv" :: Text)) experiment.verilogDesign

  TIO.writeFile
    (localExperimentDir </> ("full_output.txt" :: Text))
    result.fullOutput

  whenJust result.counterExample $
    TIO.writeFile
      (localExperimentDir </> ("counter_example.txt" :: Text))

  TIO.writeFile
    (localExperimentDir </> ("info.txt" :: Text))
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
