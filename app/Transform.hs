{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Transform
  ( -- * Transformation datatype
    Transformation (),
    applyAnyTransformation,
    applyNSPTransformation,
    applySPTransformation,

    -- * Combinators for Transformation
    possibly,
    anywherePossibly,

    -- * Transformations
    or0,
    or1,
    invertCondition,
    doubleInvertCondition,
  )
where

import Data.Generics.Uniplate.Data (Biplate, transformBiM)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Range
import Verismith.Verilog

data SemanticsPreserving
  = -- | Semantics-Preserving
    SP
  | -- | Non-Semantics-Preserving
    NSP

newtype Transformation (p :: SemanticsPreserving) b = Transformation b
  deriving (Functor)

applyAnyTransformation :: Transformation p f -> f
applyAnyTransformation (Transformation f) = f

-- | Type-restricted version of `applyAnyTransformation`
applySPTransformation :: Transformation 'SP f -> f
applySPTransformation = applyAnyTransformation

-- | Type-restricted version of `applyAnyTransformation`
applyNSPTransformation :: Transformation 'NSP f -> f
applyNSPTransformation = applyAnyTransformation

possibly :: Double -> (a -> a) -> (a -> Gen a)
possibly chance f a = do
  shouldApply <- (< chance) <$> Hog.realFloat (Range.constant 0 1)
  return $
    if shouldApply
      then f a
      else a

-- | Make a transformation apply anywhere in the AST with the specified chance (0 to 1)
anywherePossibly ::
  Biplate from to =>
  Double ->
  Transformation p (to -> to) ->
  Transformation p (from -> Gen from)
anywherePossibly chance = fmap (transformBiM . possibly chance)

doubleInvertCondition :: Transformation 'SP (Expr -> Expr)
doubleInvertCondition = Transformation $ \case
  (Cond e t f) -> Cond (UnOp UnNot (UnOp UnNot e)) t f
  e -> e

or0 :: Transformation 'SP (Expr -> Expr)
or0 = Transformation $ \e -> BinOp e BinOr (Number 0)

invertCondition :: Transformation 'NSP (Expr -> Expr)
invertCondition = Transformation $ \case
  (Cond e t f) -> Cond (UnOp UnNot e) t f
  e -> e

or1 :: Transformation 'NSP (Expr -> Expr)
or1 = Transformation $ \e -> BinOp e BinOr (Number 1)
