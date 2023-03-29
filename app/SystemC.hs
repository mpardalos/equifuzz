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
import Optics (A_Getter, LabelOptic, Lens', lens, to)
import Optics.Label (LabelOptic (labelOptic))
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    comma,
    defaultLayoutOptions,
    hsep,
    indent,
    layoutPretty,
    line,
    nest,
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

instance
  (Annotation ann, AnnExpr ann ~ annType) =>
  LabelOptic "annotation" A_Getter (Expr ann) (Expr ann) annType annType
  where
  labelOptic = to (getField @"annotation")

data Statement ann
  = Return (AnnStatement ann) (Expr ann)
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
  getField (Block ann _) = ann

-- | SystemC types. Constructor parameters correspond to template arguments
data SCType = SCInt Int | SCUInt Int
  deriving (Eq, Show, Generic, Data)

width :: Lens' SCType Int
width = lens get set
  where
    get (SCInt n) = n
    get (SCUInt n) = n

    set (SCInt _) n = SCInt n
    set (SCUInt _) n = SCUInt n

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
includeHeader = "#include <systemc.h>"

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

bComment :: Doc a -> Doc a
bComment b = "/*" <+> b <+> "*/"

instance Annotation ann => Pretty (Expr ann) where
  pretty (Constant ann n) = bComment (pretty ann) <+> pretty n
  pretty (BinOp ann l op r) =
    parens $
      bComment (pretty ann)
        <+> ( parens . hsep $
                [ pretty l,
                  pretty op,
                  pretty r
                ]
            )
  pretty (Conditional ann cond tBranch fBranch) =
    parens $
      bComment (pretty ann)
        <+> ( parens . nest 4 . sep $
                [ pretty cond,
                  "?" <+> pretty tBranch,
                  ":" <+> pretty fBranch
                ]
            )
  pretty (Variable ann name) =
    parens $
      bComment (pretty ann) <+> pretty name
  pretty (Cast ann castType expr) =
    parens $
      bComment (pretty ann)
        <+> pretty castType <> parens (pretty expr)

instance Annotation ann => Source (Expr ann) where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty

prettyBlock :: Annotation ann => [Statement ann] -> Doc a
prettyBlock statements = vsep ["{", indent 4 . vsep $ pretty <$> statements, "}"]

instance Annotation ann => Pretty (Statement ann) where
  pretty (Return ann e) = "return" <+> pretty e <> ";" <+> "//" <+> pretty ann
  pretty (Block ann statements) = pretty statements <+> "//" <+> pretty ann

instance Annotation ann => Source (Statement ann) where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty

instance Pretty SCType where
  pretty (SCInt size) = "sc_dt::sc_int<" <> pretty size <> ">"
  pretty (SCUInt size) = "sc_dt::sc_uint<" <> pretty size <> ">"

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
