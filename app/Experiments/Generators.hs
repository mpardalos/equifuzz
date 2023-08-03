{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiments.Generators (mkSystemCConstantExperiment) where

import Control.Monad (void)
import Control.Monad.Random.Strict (evalRandIO)
import Data.Either (fromRight)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID
import Experiments.Types
import GenSystemC (GenConfig, GenerateProcess (..), genSystemCConstant, Reducible(value))
import Shelly qualified as Sh
import SystemC qualified as SC

-- | Make an experiment using the SystemC-constant generator. Needs to have
-- icarus verilog (`iverilog`) available locally
mkSystemCConstantExperiment :: GenConfig -> IO Experiment
mkSystemCConstantExperiment config = do
  reducible <- evalRandIO $ genSystemCConstant config "dut"
  let (GenerateProcess {seed, transformations}, systemcModule) = reducible.value

  let wrapperName = "impl"
  let design =
        DesignSource
          { topName = wrapperName,
            source =
              SC.includeHeader
                <> "\n\n"
                <> SC.genSource systemcModule
                <> "\n\n"
                <> systemCHectorWrapper wrapperName systemcModule
          }

  comparisonValue <- simulateSystemCConstant systemcModule

  uuid <- UUID.nextRandom
  return
    Experiment
      { uuid,
        expectedResult = True,
        design,
        designDescription =
          T.unlines
            ( T.pack ("0) " ++ show seed)
                : [ T.pack (show n) <> ") " <> T.pack (show t)
                    | (n, t) <- zip [1 :: Int ..] transformations
                  ]
            ),
        comparisonValue
      }

-- | When doing equivalence checking with Hector (VC Formal) the code under test
-- needs to be presented to hector using a wrapper
systemCHectorWrapper :: SC.Annotation ann => Text -> SC.FunctionDeclaration ann -> Text
systemCHectorWrapper wrapperName SC.FunctionDeclaration {returnType, args, name} =
  [__i|
      \#include<Hector.h>

      void #{wrapperName}() {
          #{inputDeclarations}
          #{outType} out;

          #{inputsHectorRegister}
          Hector::registerOutput("out", out);

          Hector::beginCapture();
          out = #{name}(#{argList});
          Hector::endCapture();
      }

      |]
  where
    inputDeclarations :: Text
    inputDeclarations =
      T.intercalate
        "\n    "
        [ SC.genSource argType <> " " <> argName <> ";"
          | (argType, argName) <- args
        ]

    outType :: Text
    outType = SC.genSource returnType

    inputsHectorRegister :: Text
    inputsHectorRegister =
      T.intercalate
        "\n    "
        [ "Hector::registerInput(\"" <> argName <> "\", " <> argName <> ");"
          | (_, argName) <- args
        ]

    argList :: Text
    argList = T.intercalate ", " [argName | (_, argName) <- args]

-- | Run the SystemC function and return its output represented as text of a
-- Verilog-style constant. We use this output format because the normal
-- (decimal) output makes the representation of the output non-obvious. E.g. -1
-- as an sc_uint<8> or a sc_fixed<10,3> are represented completely differently.
-- With the default SystemC output, we would get "-1" in both cases, but here we
-- (correctly) get "8'hff" and "10'h380"
simulateSystemCConstant :: SC.Annotation ann => SC.FunctionDeclaration ann -> IO Text
simulateSystemCConstant decl@SC.FunctionDeclaration {returnType, name} = Sh.shelly . Sh.silently $ do
  let widthExprOrWidth :: Either Text Int = case returnType of
        SC.SCInt n -> Right n
        SC.SCUInt n -> Right n
        SC.SCFixed w _ -> Right w
        SC.SCUFixed w _ -> Right w
        SC.SCFxnumSubref -> Left [i|#{name}().length()|]
        SC.SCIntSubref -> Left [i|#{name}().length()|]
        SC.SCUIntSubref -> Left [i|#{name}().length()|]
        SC.SCIntBitref -> Right 1
        SC.SCUIntBitref -> Right 1
        SC.CUInt -> Right 32
        SC.CInt -> Right 32
        SC.CDouble -> Right 32
        SC.CBool -> Right 1

  let showWidth :: Text = case widthExprOrWidth of
        Left e -> [i|std::cout << #{e} << std::endl;|]
        Right _ -> "std::cout << 0 << std::endl;"

  let showValue :: Text =
        if SC.supportsToString returnType
          then [i|std::cout << #{name}().to_string(sc_dt::SC_HEX, false) << std::endl;|]
          else [i|std::cout << std::hex << #{name}() << std::endl;|]

  let fullSource =
        [__i|
            #{SC.includeHeader}
            \#include <iostream>

            #{SC.genSource decl}

            int sc_main(int argc, char **argv) {
                #{showWidth}
                #{showValue}
                return 0;
            }
            |]

  tmpDir <- T.strip <$> Sh.run "mktemp" ["-d"]
  let cppPath = tmpDir <> "/main.cpp"
  let binPath = tmpDir <> "/main"

  Sh.writefile (T.unpack cppPath) fullSource
  _compileOutput <- Sh.bash "g++" ["-std=c++11", "-I/usr/include/systemc", "-lsystemc", cppPath, "-o", binPath]
  programOut <- T.strip <$> Sh.bash (T.unpack binPath) []
  void $ Sh.bash "rm" ["-r", tmpDir]

  let [reportedWidth, reportedValue] = T.lines programOut

  let width = fromRight reportedWidth (T.pack . show <$> widthExprOrWidth)
  let value = T.replace "." "" reportedValue

  return (width <> "'h" <> value)
