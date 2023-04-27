{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SystemC where

import Data.Data (Data, Typeable)
import Data.Kind (Constraint, Type)
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
import Verismith.Verilog.CodeGen (Source (..))

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

data BinOp = Plus | Minus | Multiply | Divide | BitwiseOr
  deriving (Eq, Show, Generic, Data)

data Expr ann
  = Constant (AnnExpr ann) Int
  | BinOp (AnnExpr ann) (Expr ann) BinOp (Expr ann)
  | Conditional (AnnExpr ann) (Expr ann) (Expr ann) (Expr ann)
  | Variable (AnnExpr ann) Text
  | Cast (AnnExpr ann) SCType (Expr ann)
  | Range (AnnExpr ann) (Expr ann) Int Int
  deriving (Generic)

deriving instance (Annotation ann, AnnConstraint Eq ann) => Eq (Expr ann)

deriving instance (Annotation ann, AnnConstraint Show ann) => Show (Expr ann)

deriving instance (Annotation ann, AnnConstraint Data ann) => Data (Expr ann)

instance (Annotation ann, AnnExpr ann ~ annType) => HasField "annotation" (Expr ann) annType where
  getField (Constant ann _) = ann
  getField (BinOp ann _ _ _) = ann
  getField (Conditional ann _ _ _) = ann
  getField (Variable ann _) = ann
  getField (Cast ann _ _) = ann
  getField (Range ann _ _ _) = ann

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

-- | SystemC types. Constructor parameters correspond to template arguments
data SCType
  = SCInt Int
  | SCUInt Int
  | SCFixed {w :: Int, i :: Int}
  | SCUFixed {w :: Int, i :: Int}
  | SCFxnumSubref
  | SCIntSubref
  | SCUIntSubref
  | CUInt
  | CInt
  deriving (Eq, Show, Generic, Data)

isSigned :: SCType -> Bool
isSigned SCInt {} = True
isSigned SCFixed {} = True
isSigned CInt = True
-- TODO: SCSubref includes both signed and unsigned types. Add the "real" subref
-- types
isSigned SCFxnumSubref = False
isSigned SCIntSubref = False
isSigned SCUIntSubref = False
isSigned SCUInt {} = False
isSigned SCUFixed {} = False
isSigned CUInt = False

-- | `Just <subref type>` if the type supports the range operator, or `Nothing`
-- if it does not
supportsRange :: SCType -> Maybe SCType
supportsRange SCInt {} = Just SCIntSubref
supportsRange SCUInt {} = Just SCUIntSubref
supportsRange SCFixed {} = Just SCFxnumSubref
supportsRange SCUFixed {} = Just SCFxnumSubref
supportsRange SCFxnumSubref = Nothing
supportsRange SCIntSubref = Nothing
supportsRange SCUIntSubref = Nothing
supportsRange CInt = Nothing
supportsRange CUInt = Nothing

-- | Give the bitwidth of the type where that exists (i.e. SystemC types with a
-- width template parameters)
specifiedWidth :: SCType -> Maybe Int
specifiedWidth (SCInt n) = Just n
specifiedWidth SCFixed {w} = Just w
specifiedWidth (SCUInt n) = Just n
specifiedWidth SCUFixed {w} = Just w
specifiedWidth SCFxnumSubref = Nothing
specifiedWidth SCIntSubref = Nothing
specifiedWidth SCUIntSubref = Nothing
specifiedWidth CInt = Nothing
specifiedWidth CUInt = Nothing

data FunctionDeclaration ann = FunctionDeclaration
  { returnType :: SCType,
    name :: Text,
    args :: [(SCType, Text)],
    body :: [Statement ann]
  }
  deriving (Generic)

deriving instance (Annotation ann, AnnConstraint Eq ann) => Eq (FunctionDeclaration ann)

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

annComment :: Doc a -> Doc a
annComment ann = "/* " <> ann <> " */"

instance Annotation ann => Pretty (Expr ann) where
  pretty (Constant ann n) = pretty n <+> annComment (pretty ann)
  pretty (BinOp ann l op r) =
    align $
      vsep
        [ "(",
          indent 4 . vsep $
            [ pretty l,
              pretty op <+> annComment (pretty ann),
              pretty r
            ],
          ")"
        ]
  pretty (Conditional ann cond tBranch fBranch) =
    align $
      vsep
        [ "(",
          indent 4 . vsep $
            [ pretty cond <+> annComment ("?:" <+> pretty ann),
              "?" <+> pretty tBranch,
              ":" <+> pretty fBranch
            ],
          ")"
        ]
  pretty (Variable ann name) =
    pretty name <+> annComment (pretty ann)
  pretty (Cast ann castType expr) =
    pretty castType <> parens (pretty expr)
  pretty (Range ann e hi lo) = pretty e <+> ".range(" <> pretty hi <> ", " <> pretty lo <> ")"

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
  pretty SCFxnumSubref = "sc_dt::sc_fxnum_subref"
  pretty SCIntSubref = "sc_dt::sc_int_subref"
  pretty SCUIntSubref = "sc_dt::sc_uint_subref"
  pretty CInt = "int"
  pretty CUInt = "unsigned"

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
