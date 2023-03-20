module BuildOut (buildOutModules) where

import BuildOut.Verilog
import Hedgehog (Gen)
import Verismith.Verilog.AST qualified as V

buildOutModules :: V.Identifier -> V.Identifier -> Gen (V.ModDecl BuildOut, V.ModDecl BuildOut)
buildOutModules name1 name2 = do
  ((expr1, expr2), ports) <- runBuildOutM (inequivalent >>= grow)
  return
    ( singleExprModule name1 ports expr1,
      singleExprModule name2 ports expr2
    )
