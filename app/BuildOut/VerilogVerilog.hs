module BuildOut.VerilogVerilog where

import BuildOut.Internal
import BuildOut.Verilog
import Control.Monad (guard)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (view)
import Verismith.Verilog (BinaryOperator (..), Expr (..))

type ExprPair = (Expr BuildOut, Expr BuildOut)

inequivalent :: BuildOutM ExprPair
inequivalent =
  Hog.frequency
    [ (7, differentSignedShift),
      (2, differentInputs),
      (1, differentConstants)
    ]

-- | Increase the size and complexity of an expression while preserving its semantics
grow :: ExprPair -> BuildOutM ExprPair
grow pair = do
  count <-Hog.int (Hog.Range.linear 1 20)
  iterateM count grow1 pair
  where
    grow1 (e1, e2) = do
      -- FIXME: Make the random parts be identical for both expressions
      f <-
        Hog.element
          [ ifFalse,
            ifTrue,
            pure . signedUnsigned,
            pure . unsignedSigned,
            or0
            -- TODO: Implement bit-select entire vector
          ]
      e1' <- f e1
      e2' <- f e2
      return (e1', e2')

differentConstants :: BuildOutM ExprPair
differentConstants = do
  n1 <- Hog.int (Hog.Range.constant 0 255)
  n2 <- Hog.int (Hog.Range.constant 0 255)
  guard (n1 /= n2)
  return
    ( Number "differentConstants" (fromIntegral n1),
      Number "differentConstants" (fromIntegral n2)
    )

differentInputs :: BuildOutM ExprPair
differentInputs = do
  size <- wireSize
  id1 <- view #name <$> newPort size
  id2 <- view #name <$> newPort size
  return
    ( Id "differentInputs" id1,
      Id "differentInputs" id2
    )

differentSignedShift :: BuildOutM ExprPair
differentSignedShift = do
  size <- wireSize
  ident <- view #name <$> newPort size
  return
    ( concatSingle "" (BinOp "differentSignedShift" (Appl "" "$signed" (Id "" ident)) BinASR (Number "" 1)),
      concatSingle "" (BinOp "differentSignedShift" (Appl "" "$unsigned" (Id "" ident)) BinASR (Number "" 1))
    )
