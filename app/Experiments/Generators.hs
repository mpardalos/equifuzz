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
import Data.Maybe (fromJust)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID
import Experiments.Types
import GenSystemC (GenConfig, GenerateProcess (..), genSystemCConstant)
import Optics ((<&>))
import Shelly qualified as Sh
import SystemC qualified as SC

-- | Make an experiment using the SystemC-constant generator. Needs to have
-- icarus verilog (`iverilog`) available locally
mkSystemCConstantExperiment :: GenConfig -> IO Experiment
mkSystemCConstantExperiment config = do
  (GenerateProcess seed transformations, systemcModule) <- evalRandIO $ genSystemCConstant config "dut"
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

  expectedResult <-
    simulateSystemCConstant systemcModule
      <&> T.drop 2 -- 0b prefix
      <&> T.replace "." "" -- Remove decimal point if present

  -- FIXME: The error from this `fromJust` is unhandled up the stack. Handle
  -- errors from experiment generation
  let outWidth = fromJust $ SC.specifiedWidth systemcModule.returnType
  let comparisonValue = T.pack (show outWidth) <> "'b" <> expectedResult

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

-- | Run the SystemC function and return its output represented as text of a binary number
-- We use this output format because the normal (decimal) output makes the
-- representation of the output non-obvious. E.g. -1 as an sc_uint<8> or a
-- sc_fixed<10,3> are represented completely differently. With the default
-- SystemC output, we would get "-1" in both cases, but here we (correctly) get
-- "0b11111111" and "0b111.0000000"
simulateSystemCConstant :: SC.Annotation ann => SC.FunctionDeclaration ann -> IO Text
simulateSystemCConstant decl@SC.FunctionDeclaration {name} = Sh.shelly . Sh.silently $ do
  let fullSource =
        [__i|
            #{SC.includeHeader}

            #{SC.genSource decl}

            int sc_main(int argc, char **argv) {
                std::cout << #{name}().to_string(sc_dt::SC_BIN) << std::endl;
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
  return programOut
