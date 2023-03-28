module BuildOut (buildOutVerilogVerilog, buildOutSystemCVerilog) where

import BuildOut.Internal (inputPortAsSystemC, inputPortAsVerilog, maxWireSize, runBuildOutM)
import BuildOut.SystemC qualified as SC
import BuildOut.SystemCVerilog qualified as SCxV
import BuildOut.Verilog qualified as V
import BuildOut.VerilogVerilog qualified as VxV
import Data.Text (Text)
import Hedgehog (Gen)
import SystemC as SC
import Verismith.Verilog.AST qualified as V

buildOutVerilogVerilog :: V.Identifier -> V.Identifier -> Gen (V.ModDecl V.BuildOut, V.ModDecl V.BuildOut)
buildOutVerilogVerilog name1 name2 = do
  ((expr1, expr2), ports) <- runBuildOutM (VxV.inequivalent >>= VxV.grow)
  return
    ( V.singleExprModule name1 (map inputPortAsVerilog ports) expr1,
      V.singleExprModule name2 (map inputPortAsVerilog ports) expr2
    )

buildOutSystemCVerilog :: Text -> V.Identifier -> Gen (SC.FunctionDeclaration SC.BuildOut, V.ModDecl V.BuildOut)
buildOutSystemCVerilog name1 name2 = do
  ((systemcExpr, verilogExpr), ports) <- runBuildOutM (SCxV.inequivalent >>= SCxV.grow)
  return
    ( SC.singleExprFunction (SC.SCUInt maxWireSize) name1 (map inputPortAsSystemC ports) systemcExpr,
      V.singleExprModule name2 (map inputPortAsVerilog ports) verilogExpr
    )
