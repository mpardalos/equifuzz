module BuildOut (buildOutVerilogVerilog, buildOutSystemCVerilog, buildOutSystemCConstant) where

import BuildOut.Internal (BuildOutState (..), initState, inputPortAsSystemC, inputPortAsVerilog, maxWireSize, runBuildOutM)
import BuildOut.SystemC qualified as SC
import BuildOut.SystemCConstant qualified as SCConst
import BuildOut.SystemCVerilog qualified as SCxV
import BuildOut.Verilog qualified as V
import BuildOut.VerilogVerilog qualified as VxV
import Data.Text (Text)
import Hedgehog (Gen)
import Optics (traversed, (%), (^..))
import SystemC as SC
import Verismith.Verilog.AST qualified as V

buildOutVerilogVerilog :: V.Identifier -> V.Identifier -> Gen (V.ModDecl V.BuildOut, V.ModDecl V.BuildOut)
buildOutVerilogVerilog name1 name2 = do
  ((expr1, expr2), state) <- runBuildOutM (VxV.inequivalent >>= VxV.grow) (initState ())
  return
    ( V.singleExprModule name1 (map inputPortAsVerilog state.inputPorts) maxWireSize expr1,
      V.singleExprModule name2 (map inputPortAsVerilog state.inputPorts) maxWireSize expr2
    )

buildOutSystemCVerilog :: Text -> V.Identifier -> Gen (SC.FunctionDeclaration SC.BuildOut, V.ModDecl V.BuildOut)
buildOutSystemCVerilog name1 name2 = do
  ((systemcExpr, verilogExpr), state) <- runBuildOutM (SCxV.inequivalent >>= SCxV.grow) (initState ())
  return
    ( SC.singleExprFunction (SC.SCUInt maxWireSize) name1 (map inputPortAsSystemC state.inputPorts) systemcExpr,
      V.singleExprModule name2 (map inputPortAsVerilog state.inputPorts) maxWireSize verilogExpr
    )

buildOutSystemCConstant :: Text -> Gen (SC.FunctionDeclaration SC.BuildOut)
buildOutSystemCConstant name = do
  (expr, state) <- runBuildOutM SCConst.genExpr (initState SCConst.initSCConstState)
  return $
    SC.FunctionDeclaration
      { returnType = expr.annotation,
        name,
        args = map inputPortAsSystemC state.inputPorts,
        body = state.extraState.statements ++ [SC.Return () expr]
      }
