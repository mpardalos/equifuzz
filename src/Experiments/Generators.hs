{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# HLINT ignore "Use <$>" #-}

module Experiments.Generators (mkSystemCConstantExperiment, generateProcessToExperiment) where

import Control.Monad (replicateM)
import Control.Monad.Random.Strict (MonadRandom (getRandom), evalRandIO)
import Data.Either (fromRight)
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments.Types
import GenSystemC (
  GenConfig (..),
  GenerateProcess (..),
  Reducible,
  genSystemC,
  generateFromProcess,
 )
import Optics
import Safe (foldr1May)
import System.Environment.Blank (getEnvDefault)
import System.Process (readProcessWithExitCode)
import SystemC qualified as SC
import Util (runBash)

-- | Make an experiment using the SystemC-constant generator. Needs to have
-- icarus verilog (`iverilog`) available locally
mkSystemCConstantExperiment :: GenConfig -> IO (Reducible (IO Experiment))
mkSystemCConstantExperiment cfg =
  fmap (fmap (generateProcessToExperiment cfg)) $
    evalRandIO (genSystemC cfg)

-- | Modify a function so that it returns zero on inputs not in the provided
-- list of evaluations.  For inputs within the list of evaluations, it runs the
-- same code as the original function.
limitToEvaluations :: [Evaluation] -> SC.FunctionDeclaration -> SC.FunctionDeclaration
limitToEvaluations evals decl =
  let
    goodInputs = map (view #inputs) evals
    goodInputsLabelled = map (zip decl.args) goodInputs
    inputChecks = map (map (\((t, name), value) -> SC.BinOp SC.CBool (SC.Variable t name) SC.Equals (comparisonValueAsSC t value))) goodInputsLabelled
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
        (SC.Return (SC.Constant decl.returnType 0))
        Nothing
   in
    decl{SC.body = earlyReturn : decl.body}

generateProcessToExperiment :: GenConfig -> GenerateProcess -> IO Experiment
generateProcessToExperiment cfg process@GenerateProcess{seed, transformations} = do
  let rawDesign = generateFromProcess cfg "dut" process

  inputss <-
    replicateM cfg.evaluations $
      mapM (comparisonValueOfType . fst) rawDesign.args

  (outputs, hasUBs, extraInfos) <-
    unzip3 <$> mapM (simulateSystemCAt rawDesign) inputss

  let knownEvaluations = [Evaluation{..} | (inputs, output) <- zip inputss outputs]
  let hasUB = or hasUBs
  let extraInfo = T.intercalate "-\n" extraInfos

  let scDesign = limitToEvaluations knownEvaluations rawDesign

  let verilogDesign = verilogImplForEvals scDesign knownEvaluations

  experimentId <- newExperimentId
  return
    Experiment
      { experimentId
      , expectedResult = not hasUB -- Expect a negative result for programs with UB
      , scDesign
      , verilogDesign
      , size = length transformations
      , longDescription =
          T.unlines . concat $
            [ [T.pack ("Seed: " ++ show seed)]
            , [ T.pack (show n) <> " " <> T.pack (show t)
              | (n, t) <- zip [0 :: Int ..] transformations
              ]
            , [extraInfo]
            ]
      , knownEvaluations
      }

comparisonValueOfType :: MonadRandom m => SC.SCType -> m ComparisonValue
comparisonValueOfType t = case SC.knownWidth t of
  Nothing -> mkComparisonValueWord 32 <$> getRandom
  Just w -> mkComparisonValueWord w <$> getRandom

-- | Run the SystemC function and return its output represented as text of a
-- Verilog-style constant. We use this output format because the normal
-- (decimal) output makes the representation of the output non-obvious. E.g. -1
-- as an sc_uint<8> or a sc_fixed<10,3> are represented completely differently.
-- With the default SystemC output, we would get "-1" in both cases, but here we
-- (correctly) get "8'b11111111" and "10'b1110000000"
simulateSystemCAt :: SC.FunctionDeclaration -> [ComparisonValue] -> IO (ComparisonValue, Bool, Text)
simulateSystemCAt decl@SC.FunctionDeclaration{returnType, name} inputs = do
  let callArgs = T.intercalate ", " (map comparisonValueAsC inputs)
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
  _compileOutput <- readProcessWithExitCode clangPath ["-fsanitize=undefined", "-I", systemcIncludePath, "-L", systemcLibraryPath, "-lsystemc", T.unpack cppPath, "-o", T.unpack binPath] ""
  (_programExit, T.pack -> programOut, T.pack -> programStderr) <- readProcessWithExitCode (T.unpack binPath) [] ""
  let hasUndefinedBehaviour = "undefined-behavior" `T.isInfixOf` programStderr
      extraInfo =
        if hasUndefinedBehaviour
          then T.unlines ["stderr output:", "", programStderr]
          else ""
  void $ runBash ("rm -r " <> tmpDir)

  let (reportedWidth, reportedValue) =
        case T.lines programOut of
          [line1, line2] -> (line1, line2)
          ls -> error ("Unexpected output line count from SystemC program: " ++ show (length ls))

  let width = fromRight (read . T.unpack $ reportedWidth) widthExprOrWidth
  let value = T.replace "." "" reportedValue

  return
    ( mkComparisonValueWithWidth width value
    , hasUndefinedBehaviour
    , extraInfo
    )

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
          out = 0;
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
