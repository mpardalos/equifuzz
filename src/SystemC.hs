{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SystemC where

import Data.Data (Data, Typeable)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Optics (A_Getter, LabelOptic, to, makeFieldLabelsNoPrefix)
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
  deriving (Eq, Show, Generic, Data, Ord, Bounded, Enum)

data UnaryOp
  = PreIncrement
  | PreDecrement
  | PostIncrement
  | PostDecrement
  deriving (Eq, Show, Generic, Data, Ord, Bounded, Enum)

data Expr ann
  = Constant (AnnExpr ann) Int
  | BinOp (AnnExpr ann) (Expr ann) BinOp (Expr ann)
  | UnaryOp (AnnExpr ann) UnaryOp (Expr ann)
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
  getField (UnaryOp ann _ _) = ann
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

type VarName = Text

data Statement ann
  = Return (AnnStatement ann) (Expr ann)
  | AssignmentDeclaration (AnnStatement ann) SCType VarName (Expr ann)
  | Declaration (AnnStatement ann) SCType VarName
  | Assignment (AnnStatement ann) VarName (Expr ann)
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
  getField (AssignmentDeclaration ann _ _ _) = ann
  getField (Declaration ann _ _) = ann
  getField (Assignment ann _ _) = ann
  getField (Block ann _) = ann

-- | SystemC types. Parameters include template parameters as well as extra
-- information that is useful to track, but will not be present in the source
-- (e.g. width for subref types)
data SCType
  = SCInt Int
  | SCUInt Int
  | SCBigInt Int
  | SCBigUInt Int
  | SCFixed {w :: Int, i :: Int}
  | SCUFixed {w :: Int, i :: Int}
  | SCFxnumSubref {width :: Int}
  | SCIntSubref {width :: Int}
  | SCUIntSubref {width :: Int}
  | SCSignedSubref {width :: Int}
  | SCUnsignedSubref {width :: Int}
  | SCIntBitref
  | SCUIntBitref
  | SCSignedBitref
  | SCUnsignedBitref
  | CUInt
  | CInt
  | CDouble
  | CBool
  deriving (Eq, Show, Ord, Generic, Data)

-- | SystemC reduction operators (@.and_reduce()@, @.or_reduce()@, etc.)
data ReductionOperation
  = ReduceAnd
  | ReduceNand
  | ReduceOr
  | ReduceNor
  | ReduceXor
  | ReduceXNor
  deriving (Eq, Show, Ord, Generic, Data, Enum, Bounded)

allReductions :: [ReductionOperation]
allReductions = [minBound .. maxBound]

data SCTypeFlags = SCTypeFlags
  { scInt :: Bool,
    scUInt :: Bool,
    scBigInt :: Bool,
    scBigUInt :: Bool,
    scFixed :: Bool,
    scUFixed :: Bool,
    scFxnumSubref :: Bool,
    scIntSubref :: Bool,
    scUIntSubref :: Bool,
    scSignedSubref :: Bool,
    scUnsignedSubref :: Bool,
    scIntBitref :: Bool,
    scUIntBitref :: Bool,
    scSignedBitref :: Bool,
    scUnsignedBitref :: Bool,
    cUInt :: Bool,
    cInt :: Bool,
    cDouble :: Bool,
    cBool :: Bool
  }
  deriving (Eq, Show, Ord, Generic)

noTypes :: SCTypeFlags
noTypes =
  SCTypeFlags
    { scInt = False,
      scUInt = False,
      scBigInt = False,
      scBigUInt = False,
      scFixed = False,
      scUFixed = False,
      scFxnumSubref = False,
      scIntSubref = False,
      scUIntSubref = False,
      scSignedSubref = False,
      scUnsignedSubref = False,
      scIntBitref = False,
      scUIntBitref = False,
      scSignedBitref = False,
      scUnsignedBitref = False,
      cUInt = False,
      cInt = False,
      cDouble = False,
      cBool = False
    }

allTypes :: SCTypeFlags
allTypes =
  SCTypeFlags
    { scInt = True,
      scUInt = True,
      scBigInt = True,
      scBigUInt = True,
      scFixed = True,
      scUFixed = True,
      scFxnumSubref = True,
      scIntSubref = True,
      scUIntSubref = True,
      scSignedSubref = True,
      scUnsignedSubref = True,
      scIntBitref = True,
      scUIntBitref = True,
      scSignedBitref = True,
      scUnsignedBitref = True,
      cUInt = True,
      cInt = True,
      cDouble = True,
      cBool = True
    }

allSCTypes :: SCTypeFlags
allSCTypes =
  allTypes
    { cUInt = False,
      cInt = False,
      cDouble = False,
      cBool = False
    }

-- | Possible operations on a SystemC type
data Operations = Operations
  { -- | Result of the bit select operator (@x[10]@), if that is available
    bitSelect :: Maybe SCType,
    -- | Increment (++) and decrement (--)
    incrementDecrement :: Bool,
    -- | Result of the bit select operator (@x.range(10, 2)@), if that is available
    partSelect :: Maybe (Int -> SCType),
    -- | Available reduction operators. See `ReductionOperation` and `reductionMethod`.
    reductions :: [ReductionOperation],
    -- | Implicit cast operators (e.g. @operator int() const@)
    implicitCasts :: [SCType],
    -- | Types that can be constructed from this
    constructorInto :: SCTypeFlags,
    -- | Types to which values of this type can be assigned
    assignTo :: SCTypeFlags
  }
  deriving Generic

reductionMethod :: ReductionOperation -> Text
reductionMethod ReduceAnd = "and_reduce"
reductionMethod ReduceNand = "nand_reduce"
reductionMethod ReduceOr = "or_reduce"
reductionMethod ReduceNor = "nor_reduce"
reductionMethod ReduceXor = "xor_reduce"
reductionMethod ReduceXNor = "xnor_reduce"

-- | Get all possible operations for a SystemC type
operations :: SCType -> Operations
operations SCBigInt {} =
  Operations
    { bitSelect = Just SCSignedBitref,
      partSelect = Just SCSignedSubref,
      implicitCasts = [],
      reductions = allReductions,
      incrementDecrement = True,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCBigUInt {} =
  Operations
    { bitSelect = Just SCUnsignedBitref,
      partSelect = Just SCUnsignedSubref,
      implicitCasts = [],
      reductions = allReductions,
      incrementDecrement = True,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCInt {} =
  Operations
    { bitSelect = Just SCIntBitref,
      partSelect = Just SCIntSubref,
      implicitCasts = [CInt],
      reductions = allReductions,
      incrementDecrement = True,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCUInt {} =
  Operations
    { bitSelect = Just SCUIntBitref,
      partSelect = Just SCUIntSubref,
      implicitCasts = [CUInt],
      reductions = allReductions,
      incrementDecrement = True,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCIntSubref {} =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [CInt],
      reductions = allReductions,
      incrementDecrement = False,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCUIntSubref {} =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [CUInt],
      reductions = allReductions,
      incrementDecrement = False,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCSignedSubref {} =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [],
      reductions = allReductions,
      incrementDecrement = False,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCUnsignedSubref {} =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [],
      reductions = allReductions,
      incrementDecrement = False,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCFixed {} =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [CInt, CDouble],
      reductions = [],
      incrementDecrement = True,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCUFixed {} =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [CUInt, CDouble],
      reductions = [],
      incrementDecrement = True,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCFxnumSubref {} =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [],
      reductions = allReductions,
      incrementDecrement = False,
      constructorInto = allSCTypes,
      assignTo = allSCTypes
    }
operations SCIntBitref =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [CBool],
      reductions = [],
      incrementDecrement = False,
      constructorInto = allTypes,
      assignTo = allTypes
    }
operations SCUIntBitref =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [CBool],
      reductions = [],
      incrementDecrement = False,
      constructorInto = allTypes,
      assignTo = allTypes
    }
operations SCSignedBitref =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [CBool],
      reductions = [],
      incrementDecrement = False,
      constructorInto = allTypes,
      assignTo = allTypes
    }
operations SCUnsignedBitref =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [CBool],
      reductions = [],
      incrementDecrement = False,
      constructorInto = allTypes,
      assignTo = allTypes
    }
operations CUInt =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [],
      reductions = [],
      incrementDecrement = True,
      constructorInto = allTypes,
      assignTo = allTypes
    }
operations CInt =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [],
      reductions = [],
      incrementDecrement = True,
      constructorInto = allTypes,
      assignTo = allTypes
    }
operations CDouble =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [],
      reductions = [],
      incrementDecrement = True,
      constructorInto = allTypes,
      assignTo = allTypes
    }
operations CBool =
  Operations
    { bitSelect = Nothing,
      partSelect = Nothing,
      implicitCasts = [],
      reductions = [],
      incrementDecrement = True,
      constructorInto = allTypes,
      assignTo = allTypes
    }

-- | Give the bitwidth of the type where that exists (i.e. SystemC types with a
-- width template parameters)
knownWidth :: SCType -> Maybe Int
knownWidth (SCInt n) = Just n
knownWidth (SCBigInt n) = Just n
knownWidth SCFixed {w} = Just w
knownWidth (SCUInt n) = Just n
knownWidth (SCBigUInt n) = Just n
knownWidth SCUFixed {w} = Just w
knownWidth SCFxnumSubref {width} = Just width
knownWidth SCIntSubref {width} = Just width
knownWidth SCUIntSubref {width} = Just width
knownWidth SCSignedSubref {width} = Just width
knownWidth SCUnsignedSubref {width} = Just width
knownWidth SCIntBitref = Nothing
knownWidth SCUIntBitref = Nothing
knownWidth SCSignedBitref = Nothing
knownWidth SCUnsignedBitref = Nothing
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
  pretty (UnaryOp _ op expr) =
    parens $ case op of
      PostIncrement -> pretty expr <> "++"
      PreIncrement -> "++" <> pretty expr
      PostDecrement -> pretty expr <> "--"
      PreDecrement -> "--" <> pretty expr
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
  pretty (AssignmentDeclaration ann t name expr) =
    pretty t <+> pretty name <+> "=" <+> pretty expr <> "; //" <+> pretty ann
  pretty (Assignment ann name expr) =
    pretty name <+> "=" <+> pretty expr <> "; //" <+> pretty ann
  pretty (Declaration ann t name) =
    pretty t <+> pretty name <> "; //" <+> pretty ann
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
  pretty (SCBigInt size) = "sc_dt::sc_bigint<" <> pretty size <> ">"
  pretty (SCBigUInt size) = "sc_dt::sc_biguint<" <> pretty size <> ">"
  pretty (SCFixed w i) = "sc_dt::sc_fixed<" <> pretty w <> "," <> pretty i <> ">"
  pretty (SCUFixed w i) = "sc_dt::sc_ufixed<" <> pretty w <> "," <> pretty i <> ">"
  pretty SCFxnumSubref {} = "sc_dt::sc_fxnum_subref"
  pretty SCIntSubref {} = "sc_dt::sc_int_subref"
  pretty SCUIntSubref {} = "sc_dt::sc_uint_subref"
  pretty SCSignedSubref {} = "sc_dt::sc_signed_subref"
  pretty SCUnsignedSubref {} = "sc_dt::sc_unsigned_subref"
  pretty SCIntBitref = "sc_dt::sc_int_bitref"
  pretty SCUIntBitref = "sc_dt::sc_uint_bitref"
  pretty SCSignedBitref = "sc_dt::sc_signed_bitref"
  pretty SCUnsignedBitref = "sc_dt::sc_unsigned_bitref"
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

makeFieldLabelsNoPrefix ''SCTypeFlags
makeFieldLabelsNoPrefix ''Operations
