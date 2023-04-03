module BuildOut.SystemCVerilog where

import BuildOut.Internal
import BuildOut.SystemC qualified as SC
import BuildOut.Verilog qualified as V
import Control.Applicative (Applicative (liftA2))
import Control.Monad (guard)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (view)
import SystemC qualified as SC
import Verismith.Verilog qualified as V

type ExprPair = (SC.Expr SC.BuildOut, V.Expr V.BuildOut)

inequivalent :: BuildOutM s ExprPair
inequivalent =
  Hog.frequency
    [ (1, differentInputs),
      (1, differentConstants)
    ]

-- | Increase the size and complexity of an expression while preserving its semantics
grow :: ExprPair -> BuildOutM s ExprPair
grow (systemcExpr, verilogExpr) = do
  count <- Hog.int (Hog.Range.linear 1 20)
  liftA2
    (,)
    (iterateM count systemcGrow systemcExpr)
    (iterateM count verilogGrow verilogExpr)
  where
    systemcGrow e =
      Hog.choice
        [ SC.or0 e,
          SC.plusNMinusN e,
          SC.plus0 e,
          SC.times1 e,
          SC.ifTrue e
        ]

    verilogGrow e =
      Hog.choice
        [ V.or0 e
        ]

differentConstants :: BuildOutM s ExprPair
differentConstants = do
  n1 <- Hog.int (Hog.Range.constant 0 255)
  n2 <- Hog.int (Hog.Range.constant 0 255)
  guard (n1 /= n2)
  return
    ( SC.constant (fromIntegral n1),
      V.Number "differentConstants" (fromIntegral n2)
    )

differentInputs :: BuildOutM s ExprPair
differentInputs = do
  size <- wireSize
  id1 <- SC.newInput size
  id2 <- view #name <$> V.newPort size
  return
    ( SC.Variable (SC.SCUInt size) id1,
      V.Id "differentInputs" id2
    )
