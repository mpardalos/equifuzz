{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Transform
  ( -- * Transformation datatype
    AnnTransform,
    Transformation (),
    applyAnyTransformation,
    applyNSPTransformation,
    applySPTransformation,

    -- * Calculating reachability information
    annotateForTransformations,

    -- * Combinators for Transformation
    possibly,
    anywherePossibly,
    randomizeSP,
    randomizeNSP,

    -- * Transformations
    or0,
    or1,
    flipCondBranches,
    invertCondition,
    doubleInvertCondition,
    exprTransformsSP,
    exprTransformsNSP,
    somewhereReachable,
  )
where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (Biplate, contextsBi, transformBi, transformBiM)
import GHC.Generics (Generic)
import GHC.Records (HasField)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Range
import Verismith.Verilog
import Verismith.Verilog.AST (Annotated (setDefaultAnnotations), Annotation (..), Default (def))
import Data.Foldable (foldrM)

data SemanticsPreserving
  = -- | Semantics-Preserving
    SP
  | -- | Non-Semantics-Preserving
    NSP

newtype Transformation (p :: SemanticsPreserving) b = Transformation b
  deriving (Functor)

data AnnTransform

data Reachable = Reachable | Unreachable | Unknown
  deriving (Eq, Ord, Show, Enum, Generic, Data)

instance Default Reachable where
  def = Unknown

-- | The semigroup follows "OR" semantics
-- 1 | 0 = 1
-- X | X = X
-- 1 | x = 1
-- 0 | x = x
instance Semigroup Reachable where
  Unknown <> Unknown = Unknown
  Reachable <> _ = Reachable
  _ <> Reachable = Reachable
  Unreachable <> r = r
  r <> Unreachable = r

instance Monoid Reachable where
  mempty = Unreachable

instance Annotation AnnTransform where
  type AnnExpr AnnTransform = Reachable
  type AnnConstExpr AnnTransform = ()
  type AnnModDecl AnnTransform = ()
  type AnnStatement AnnTransform = ()
  type AnnModItem AnnTransform = ()

annotateForTransformations :: (Data (ast AnnTransform), Data (ast ann), Annotated ast) => ast ann -> ast AnnTransform
annotateForTransformations = annotateReachability . setDefaultAnnotations

annotateReachability :: Data (ast AnnTransform) => ast AnnTransform -> ast AnnTransform
annotateReachability = transformBi $ \case
  (ModCA a (ContAssign i e)) -> ModCA a (ContAssign i (annotateExpr Reachable e))
  mi -> mi

annotateExpr :: Reachable -> Expr AnnTransform -> Expr AnnTransform
annotateExpr r (Cond _ cond tBranch fBranch) = Cond r (annotateExpr r cond) (annotateExpr Unknown tBranch) (annotateExpr Unknown fBranch)
annotateExpr r (Number _ a) = Number r a
annotateExpr r (Id _ a) = Id r a
annotateExpr r (VecSelect _ a b) = VecSelect r a (annotateExpr r b)
annotateExpr r (RangeSelect _ a b) = RangeSelect r a b
annotateExpr r (Concat _ a) = Concat r (annotateExpr r <$> a)
annotateExpr r (UnOp _ a b) = UnOp r a (annotateExpr r b)
annotateExpr r (BinOp _ a b c) = BinOp r (annotateExpr r a) b (annotateExpr r c)
annotateExpr r (Appl _ a b) = Appl r a (annotateExpr r b)
annotateExpr r (Str _ a) = Str r a

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

-- | Make a transformation apply in one random, reachable place in the AST.
somewhereReachable ::
  ( Data (ast AnnTransform),
    Data to,
    HasField "annotation" to Reachable
  ) =>
  Transformation p (to -> Maybe to) ->
  Transformation p (ast AnnTransform -> Gen (ast AnnTransform))
somewhereReachable = fmap $ \trans ast -> do
  let reachableContexts = [(hole, context) | (hole, context) <- contextsBi ast, hole.annotation == Reachable]
  if not (null reachableContexts)
    then do
      (replacement, replaceExpr) <- Hog.just $ do
        (expr, replaceExpr) <- Hog.element reachableContexts
        return ((,replaceExpr) <$> trans expr)
      return (replaceExpr replacement)
    else return ast

doubleInvertCondition :: Transformation 'SP (Expr AnnTransform -> Expr AnnTransform)
doubleInvertCondition = Transformation $ \case
  (Cond a e t f) -> Cond a (UnOp a UnNot (UnOp a UnNot e)) t f
  e -> e

or0 :: Annotation ann => Transformation 'SP (Expr ann -> Expr ann)
or0 = Transformation $ \e -> BinOp e.annotation e BinOr (Number e.annotation 0)

and1 :: Annotation ann => Transformation 'SP (Expr ann -> Expr ann)
and1 = Transformation $ \e -> BinOp e.annotation e BinAnd (Number e.annotation 1)

xor0 :: Annotation ann => Transformation 'SP (Expr ann -> Expr ann)
xor0 = Transformation $ \e -> BinOp e.annotation e BinXor (Number e.annotation 0)

exprTransformsSP :: [Transformation 'SP (Expr AnnTransform -> Expr AnnTransform)]
exprTransformsSP =
  [ doubleInvertCondition,
    or0,
    and1,
    xor0
  ]

flipCondBranches :: Transformation 'NSP (Expr ann -> Maybe (Expr ann))
flipCondBranches = Transformation $ \case
  (Cond ann c t f) -> Just $ Cond ann c f t
  _ -> Nothing

invertCondition :: Transformation 'NSP (Expr ann -> Maybe (Expr ann))
invertCondition = Transformation $ \case
  (Cond ann e t f) -> Just $ Cond ann (UnOp ann UnNot e) t f
  _ -> Nothing

or1 :: Annotation ann => Transformation 'NSP (Expr ann -> Maybe (Expr ann))
or1 = Transformation $ \e -> Just $ BinOp e.annotation e BinOr (Number e.annotation 1)

exprTransformsNSP :: [Transformation 'NSP (Expr AnnTransform -> Maybe (Expr AnnTransform))]
exprTransformsNSP = [flipCondBranches, invertCondition, or1]

-- | Pick some random semantics-preserving transformations and apply them
randomizeSP :: Data a => a -> Gen a
randomizeSP val = do
  transformations <-
    Hog.list (Range.linear 1 10)
      . Hog.element
      . map (anywherePossibly 0.5)
      $ exprTransformsSP
  foldrM applySPTransformation val transformations

-- | Pick some random semantics-preserving transformations and apply them
randomizeNSP :: Data (ast AnnTransform) => ast AnnTransform -> Gen (ast AnnTransform)
randomizeNSP val = do
  transformation <- Hog.element exprTransformsNSP
  applyNSPTransformation (somewhereReachable transformation) val
