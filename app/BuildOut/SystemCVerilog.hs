module BuildOut.SystemCVerilog where

import BuildOut.Internal
import BuildOut.SystemC qualified as SC
import BuildOut.Verilog qualified as V
import Control.Monad (guard)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (view)
import SystemC qualified as SC
import Verismith.Verilog qualified as V

type ExprPair = (SC.Expr, V.Expr V.BuildOut)

inequivalent :: BuildOutM ExprPair
inequivalent =
  Hog.frequency
    [ (1, differentInputs),
      (1, differentConstants)
    ]

-- | Increase the size and complexity of an expression while preserving its semantics
grow :: ExprPair -> BuildOutM ExprPair
grow pair = do
  count <- Hog.int (Hog.Range.linear 1 20)
  iterateM count grow1 pair
  where
    grow1 (e1, e2) = do
      (systemcGrow, verilogGrow) <-
        Hog.element
          [ (SC.or0, V.or0) ]
      e1' <- systemcGrow e1
      e2' <- verilogGrow e2
      return (e1', e2')

differentConstants :: BuildOutM ExprPair
differentConstants = do
  n1 <- Hog.int (Hog.Range.constant 0 255)
  n2 <- Hog.int (Hog.Range.constant 0 255)
  guard (n1 /= n2)
  return
    ( SC.Constant (fromIntegral n1),
      V.Number "differentConstants" (fromIntegral n2)
    )

differentInputs :: BuildOutM ExprPair
differentInputs = do
  size <- wireSize
  id1 <- SC.newInput size
  id2 <- view #name <$> V.newPort size
  return
    ( SC.Variable id1,
      V.Id "differentInputs" id2
    )
