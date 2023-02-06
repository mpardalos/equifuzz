{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Transform
  ( Transformation (),
    applyAnyTransformation,
    applyNSPTransformation,
    applySPTransformation,
    doubleInvertCondition,
    or0,
    possibly,
    anywherePossibly,
  )
where

import Data.Generics.Uniplate.Data (Biplate, transformBiM)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Hog
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

possibly :: (a -> a) -> (a -> Gen a)
possibly f a = do
  shouldApply <- Hog.bool
  return $
    if shouldApply
      then f a
      else a

-- | Make a transformation apply anywhere in the AST with a 50-50 chance
anywherePossibly :: Biplate from to => Transformation p (to -> to) -> Transformation p (from -> Gen from)
anywherePossibly = fmap (transformBiM . possibly)

doubleInvertCondition :: Transformation 'SP (Expr -> Expr)
doubleInvertCondition = Transformation $ \case
  (Cond e t f) -> Cond (UnOp UnNot (UnOp UnNot e)) t f
  e -> e

or0 :: Transformation 'SP (Expr -> Expr)
or0 = Transformation $ \e -> BinOp e BinOr (Number 0)
