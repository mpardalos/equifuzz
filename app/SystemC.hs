{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SystemC where

import Data.Data (Data, Typeable)
import Data.Kind (Constraint, Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Optics (A_Getter, LabelOptic, to)
import Optics.Label (LabelOptic (labelOptic))
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    align,
    comma,
    defaultLayoutOptions,
    hsep,
    indent,
    layoutPretty,
    line,
    parens,
    punctuate,
    sep,
    vsep,
    (<+>),
  )
import Prettyprinter.Render.Text (renderStrict)

type AnnConstraint (c :: Type -> Constraint) ann =
  ( c (AnnExpr ann),
    c (AnnStatement ann)
  )

class
  ( AnnConstraint Data ann,
    AnnConstraint Show ann,
    AnnConstraint Pretty ann,
    Typeable ann,
    Typeable k
  ) =>
  Annotation (ann :: k)
  where
  type AnnExpr ann
  type AnnStatement ann

instance Annotation () where
  type AnnExpr () = ()
  type AnnStatement () = ()

class Source a where
  genSource :: a -> Text

data BinOp = Plus | Minus | Multiply | Divide | BitwiseOr
  deriving (Eq, Show, Generic, Data, Ord)

data Expr ann
  = Constant (AnnExpr ann) Int
  | BinOp (AnnExpr ann) (Expr ann) BinOp (Expr ann)
  | Conditional (AnnExpr ann) (Expr ann) (Expr ann) (Expr ann)
  | Variable (AnnExpr ann) Text
  | Cast (AnnExpr ann) SCType (Expr ann)
  | Bitref (AnnExpr ann) (Expr ann) Int
  | MethodCall (AnnExpr ann) (Expr ann) Text [Expr ann]
  deriving (Generic)

deriving instance (Annotation ann, AnnConstraint Eq ann) => Eq (Expr ann)

deriving instance (Annotation ann, AnnConstraint Ord ann) => Ord (Expr ann)

deriving instance (Annotation ann, AnnConstraint Show ann) => Show (Expr ann)

deriving instance (Annotation ann, AnnConstraint Data ann) => Data (Expr ann)

instance (Annotation ann, AnnExpr ann ~ annType) => HasField "annotation" (Expr ann) annType where
  getField (Constant ann _) = ann
  getField (BinOp ann _ _ _) = ann
  getField (Conditional ann _ _ _) = ann
  getField (Variable ann _) = ann
  getField (Cast ann _ _) = ann
  getField (Bitref ann _ _) = ann
  getField (MethodCall ann _ _ _) = ann

instance
  (Annotation ann, AnnExpr ann ~ annType) =>
  LabelOptic "annotation" A_Getter (Expr ann) (Expr ann) annType annType
  where
  labelOptic = to (getField @"annotation")

data Statement ann
  = Return (AnnStatement ann) (Expr ann)
  | Declaration (AnnStatement ann) SCType Text (Expr ann)
  | Block (AnnStatement ann) [Statement ann]
  deriving (Generic)

deriving instance (Annotation ann, AnnConstraint Eq ann) => Eq (Statement ann)

deriving instance (Annotation ann, AnnConstraint Ord ann) => Ord (Statement ann)

deriving instance (Annotation ann, AnnConstraint Show ann) => Show (Statement ann)

deriving instance (Annotation ann, AnnConstraint Data ann) => Data (Statement ann)

instance
  (Annotation ann, AnnStatement ann ~ annType) =>
  LabelOptic "annotation" A_Getter (Statement ann) (Statement ann) annType annType
  where
  labelOptic = to (getField @"annotation")

instance (annType ~ AnnStatement ann) => HasField "annotation" (Statement ann) annType where
  getField (Return ann _) = ann
  getField (Declaration ann _ _ _) = ann
  getField (Block ann _) = ann

-- | SystemC types. Parameters include template parameters as well as extra
-- information that is useful to track, but will not be present in the source
-- (e.g. width for subref types)
data SCType
  = SCInt Int
  | SCUInt Int
  | SCFixed {w :: Int, i :: Int}
  | SCUFixed {w :: Int, i :: Int}
  | SCFxnumSubref {width :: Int}
  | SCIntSubref {width :: Int}
  | SCUIntSubref {width :: Int}
  | SCIntBitref
  | SCUIntBitref
  | CUInt
  | CInt
  | CDouble
  | CBool
  deriving (Eq, Show, Ord, Generic, Data)

data SCOperation
  = BitSelect
  | PartSelect
  | ReductionOperation ReductionOperation
  deriving (Eq, Show, Ord, Generic, Data)

data ReductionOperation
  = ReduceAnd
  | ReduceNand
  | ReduceOr
  | ReduceNor
  | ReduceXor
  | ReduceXNor
  deriving (Eq, Show, Ord, Generic, Data, Enum, Bounded)

reductionMethod :: ReductionOperation -> Text
reductionMethod ReduceAnd = "and_reduce"
reductionMethod ReduceNand = "nand_reduce"
reductionMethod ReduceOr = "or_reduce"
reductionMethod ReduceNor = "nor_reduce"
reductionMethod ReduceXor = "xor_reduce"
reductionMethod ReduceXNor = "xnor_reduce"

supportedOperations :: SCType -> Set SCOperation
supportedOperations SCInt {} =
  [ BitSelect,
    PartSelect,
    ReductionOperation ReduceAnd,
    ReductionOperation ReduceNand,
    ReductionOperation ReduceOr,
    ReductionOperation ReduceNor,
    ReductionOperation ReduceXor,
    ReductionOperation ReduceXNor
  ]
supportedOperations SCUInt {} =
  [ BitSelect,
    PartSelect,
    ReductionOperation ReduceAnd,
    ReductionOperation ReduceNand,
    ReductionOperation ReduceOr,
    ReductionOperation ReduceNor,
    ReductionOperation ReduceXor,
    ReductionOperation ReduceXNor
  ]
supportedOperations SCIntSubref {} =
  [ ReductionOperation ReduceAnd,
    ReductionOperation ReduceNand,
    ReductionOperation ReduceOr,
    ReductionOperation ReduceNor,
    ReductionOperation ReduceXor,
    ReductionOperation ReduceXNor
  ]
supportedOperations SCUIntSubref {} =
  [ ReductionOperation ReduceAnd,
    ReductionOperation ReduceNand,
    ReductionOperation ReduceOr,
    ReductionOperation ReduceNor,
    ReductionOperation ReduceXor,
    ReductionOperation ReduceXNor
  ]
supportedOperations SCFixed {} = []
supportedOperations SCUFixed {} = []
supportedOperations SCFxnumSubref {} =
  [ ReductionOperation ReduceAnd,
    ReductionOperation ReduceNand,
    ReductionOperation ReduceOr,
    ReductionOperation ReduceNor,
    ReductionOperation ReduceXor,
    ReductionOperation ReduceXNor
  ]
supportedOperations SCIntBitref = []
supportedOperations SCUIntBitref = []
supportedOperations CUInt = []
supportedOperations CInt = []
supportedOperations CDouble = []
supportedOperations CBool = []

supports :: SCType -> SCOperation -> Bool
t `supports` op = op `Set.member` supportedOperations t

-- | The list of types that this type can be *implicitly* cast to, including itself
implicitCastTargetsOf :: SCType -> Set SCType
implicitCastTargetsOf t@SCInt {} = [t, CInt]
implicitCastTargetsOf t@SCUInt {} = [t, CUInt]
implicitCastTargetsOf t@SCIntSubref {} = [t, CInt]
implicitCastTargetsOf t@SCUIntSubref {} = [t, CUInt]
-- FIXME: sc_fixed -> int and sc_ufixed -> uint only exist as implicit casts in hector
implicitCastTargetsOf t@SCFixed {} = [t, CInt, CDouble]
implicitCastTargetsOf t@SCUFixed {} = [t, CUInt, CDouble]
-- FIXME: Subrefs can be implicitly cast to sc_bv_base (which has no explicit width)
implicitCastTargetsOf t@SCFxnumSubref {} = [t] -- TODO: Add bv_base
-- FIXME: This is a lie, bitrefs actually implement `operator uint64()`
implicitCastTargetsOf t@SCIntBitref = [t, CBool]
implicitCastTargetsOf t@SCUIntBitref = [t, CBool]
-- TODO: Can we say that CUInt and CInt can be implicitly cast to each other?
implicitCastTargetsOf t@CUInt = [t]
implicitCastTargetsOf t@CInt = [t]
implicitCastTargetsOf t@CDouble = [t]
implicitCastTargetsOf t@CBool = [t]

-- | `Just <subref type>` if the type supports the range operator, or `Nothing`
-- if it does not. Range bounds are needed to keep track of the width on the
-- result type. (See `SCType`)
rangeType :: SCType -> Int -> Int -> Maybe SCType
rangeType _ hi lo | hi < lo = Nothing
rangeType SCInt {} hi lo = Just (SCIntSubref (hi - lo + 1))
rangeType SCUInt {} hi lo = Just (SCUIntSubref (hi - lo + 1))
rangeType SCFixed {} hi lo = Just (SCFxnumSubref (hi - lo + 1))
rangeType SCUFixed {} hi lo = Just (SCFxnumSubref (hi - lo + 1))
rangeType SCFxnumSubref {} _ _ = Nothing
rangeType SCIntSubref {} _ _ = Nothing
rangeType SCUIntSubref {} _ _ = Nothing
rangeType SCIntBitref _ _ = Nothing
rangeType SCUIntBitref _ _ = Nothing
rangeType CInt _ _ = Nothing
rangeType CUInt _ _ = Nothing
rangeType CDouble _ _ = Nothing
rangeType CBool _ _ = Nothing

-- | `Just <subref type>` if the type supports the bitref operator, or `Nothing`
-- if it does not
bitrefType :: SCType -> Maybe SCType
bitrefType SCInt {} = Just SCIntBitref
bitrefType SCUInt {} = Just SCUIntBitref
-- TODO: Fxnum bitrefs
bitrefType SCFixed {} = Nothing
bitrefType SCUFixed {} = Nothing
bitrefType SCFxnumSubref {} = Nothing
bitrefType SCIntSubref {} = Nothing
bitrefType SCUIntSubref {} = Nothing
bitrefType SCIntBitref = Nothing
bitrefType SCUIntBitref = Nothing
bitrefType CInt = Nothing
bitrefType CUInt = Nothing
bitrefType CDouble = Nothing
bitrefType CBool = Nothing

-- | Give the bitwidth of the type where that exists (i.e. SystemC types with a
-- width template parameters)
knownWidth :: SCType -> Maybe Int
knownWidth (SCInt n) = Just n
knownWidth SCFixed {w} = Just w
knownWidth (SCUInt n) = Just n
knownWidth SCUFixed {w} = Just w
knownWidth SCFxnumSubref {width} = Just width
knownWidth SCIntSubref {width} = Just width
knownWidth SCUIntSubref {width} = Just width
knownWidth SCIntBitref = Nothing
knownWidth SCUIntBitref = Nothing
knownWidth CInt = Nothing
knownWidth CUInt = Nothing
knownWidth CDouble = Nothing
knownWidth CBool = Nothing

data FunctionDeclaration ann = FunctionDeclaration
  { returnType :: SCType,
    name :: Text,
    args :: [(SCType, Text)],
    body :: [Statement ann]
  }
  deriving (Generic)

deriving instance (Annotation ann, AnnConstraint Eq ann) => Eq (FunctionDeclaration ann)

deriving instance (Annotation ann, AnnConstraint Ord ann) => Ord (FunctionDeclaration ann)

deriving instance (Annotation ann, AnnConstraint Show ann) => Show (FunctionDeclaration ann)

deriving instance (Annotation ann, AnnConstraint Data ann) => Data (FunctionDeclaration ann)

newtype TranslationUnit ann = TranslationUnit [FunctionDeclaration ann]
  deriving (Generic)

deriving instance (Annotation ann, AnnConstraint Eq ann) => Eq (TranslationUnit ann)

deriving instance (Annotation ann, AnnConstraint Show ann) => Show (TranslationUnit ann)

deriving instance (Annotation ann, AnnConstraint Data ann) => Data (TranslationUnit ann)

-- | This needs to be included in the final program
includeHeader :: Text
includeHeader = "#define SC_INCLUDE_FX\n#include <systemc>"

instance Pretty BinOp where
  pretty Plus = "+"
  pretty Minus = "-"
  pretty Multiply = "*"
  pretty Divide = "/"
  pretty BitwiseOr = "|"

instance Source BinOp where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty

instance Pretty ReductionOperation where
  pretty ReduceAnd = "and_reduce"
  pretty ReduceNand = "nand_reduce"
  pretty ReduceOr = "or_reduce"
  pretty ReduceNor = "nor_reduce"
  pretty ReduceXor = "xor_reduce"
  pretty ReduceXNor = "xnor_reduce"

annComment :: Doc a -> Doc a
annComment ann = "/* " <> ann <> " */"

instance Annotation ann => Pretty (Expr ann) where
  pretty (Constant _ n)
    | n < 0 = parens (pretty n)
    | otherwise = pretty n
  pretty (BinOp _ l op r) =
    hsep ["(", pretty l, pretty op, pretty r, ")"]
  pretty (Conditional _ cond tBranch fBranch) =
    "("
      <> ( align . vsep $
             [ pretty cond,
               "?" <+> pretty tBranch,
               ":" <+> pretty fBranch
             ]
         )
      <> ")"
  pretty (Variable _ name) =
    pretty name
  pretty (Cast _ castType expr) =
    pretty castType <> parens (pretty expr)
  pretty (MethodCall _ e method args) =
    pretty e
      <> "."
      <> pretty method
      <> "("
      <> hsep (punctuate ", " (map pretty args))
      <> ")"
  pretty (Bitref _ e bit) = pretty e <> "[" <> pretty bit <> "]"

instance Annotation ann => Source (Expr ann) where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty

prettyBlock :: Annotation ann => [Statement ann] -> Doc a
prettyBlock statements = vsep ["{", indent 4 . vsep $ pretty <$> statements, "}"]

instance Annotation ann => Pretty (Statement ann) where
  pretty (Return ann e) =
    "return" <+> pretty e <> "; //" <+> pretty ann
  pretty (Declaration ann t name expr) =
    pretty t <+> pretty name <+> "=" <+> pretty expr <> "; //" <+> pretty ann
  pretty (Block ann statements) =
    prettyBlock statements <+> "//" <+> pretty ann

instance Annotation ann => Source (Statement ann) where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty

instance Pretty SCType where
  pretty (SCInt size) = "sc_dt::sc_int<" <> pretty size <> ">"
  pretty (SCUInt size) = "sc_dt::sc_uint<" <> pretty size <> ">"
  pretty (SCFixed w i) = "sc_dt::sc_fixed<" <> pretty w <> "," <> pretty i <> ">"
  pretty (SCUFixed w i) = "sc_dt::sc_ufixed<" <> pretty w <> "," <> pretty i <> ">"
  pretty SCFxnumSubref {} = "sc_dt::sc_fxnum_subref"
  pretty SCIntSubref {} = "sc_dt::sc_int_subref"
  pretty SCUIntSubref {} = "sc_dt::sc_uint_subref"
  pretty SCIntBitref = "sc_dt::sc_int_bitref"
  pretty SCUIntBitref = "sc_dt::sc_uint_bitref"
  pretty CInt = "int"
  pretty CUInt = "unsigned"
  pretty CDouble = "double"
  pretty CBool = "bool"

instance Source SCType where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty

instance Annotation ann => Pretty (FunctionDeclaration ann) where
  pretty FunctionDeclaration {..} =
    pretty returnType
      <+> pretty name
        <> prettyArgs
      <+> prettyBlock body
    where
      prettyArgs =
        parens . sep . punctuate comma $
          [ pretty argType <+> pretty argName
            | (argType, argName) <- args
          ]

instance Annotation ann => Source (FunctionDeclaration ann) where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty

instance Annotation ann => Pretty (TranslationUnit ann) where
  pretty (TranslationUnit funcs) =
    vsep . punctuate line $ map pretty funcs

instance Annotation ann => Source (TranslationUnit ann) where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty
