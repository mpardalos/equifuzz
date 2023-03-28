{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SystemC where

import Data.Data (Data, Typeable)
import Data.Default.Class (Default (def))
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Optics (A_Getter, LabelOptic, to)
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
  ( AnnConstraint Default ann,
    AnnConstraint Data ann,
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

prettyBinOp :: BinOp -> Doc a
prettyBinOp Plus = "+"
prettyBinOp Minus = "-"
prettyBinOp Multiply = "*"
prettyBinOp Divide = "/"
prettyBinOp BitwiseOr = "|"

instance Source BinOp where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettyBinOp

bComment :: Doc a -> Doc a
bComment b = "/*" <+> b <+> "*/"

prettyExpr :: Annotation ann => Expr ann -> Doc a
prettyExpr (Constant ann n) = bComment (pretty ann) <+> pretty n
prettyExpr (BinOp ann l op r) =
  parens $
    bComment (pretty ann)
      <+> ( parens . hsep $
              [ prettyExpr l,
                prettyBinOp op,
                prettyExpr r
              ]
          )
prettyExpr (Conditional ann cond tBranch fBranch) =
  parens $
    bComment (pretty ann)
      <+> ( parens . nest 4 . sep $
              [ prettyExpr cond,
                "?" <+> prettyExpr tBranch,
                ":" <+> prettyExpr fBranch
              ]
          )
prettyExpr (Variable ann name) =
  parens $
    bComment (pretty ann) <+> pretty name
prettyExpr (Cast ann castType expr) =
  parens $
    bComment (pretty ann)
      <+> prettySCType castType <> parens (prettyExpr expr)

instance Annotation ann => Source (Expr ann) where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettyExpr

prettyStatement :: Annotation ann => Statement ann -> Doc a
prettyStatement (Return ann e) = "return" <+> prettyExpr e <> ";" <+> "//" <+> pretty ann
prettyStatement (Block ann statements) =
  vsep ["{", indent 4 . vsep $ prettyStatement <$> statements, "}" <+> "//" <+> pretty ann]

instance Annotation ann => Source (Statement ann) where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettyStatement

prettySCType :: SCType -> Doc a
prettySCType (SCInt size) = "sc_dt::sc_int<" <> pretty size <> ">"
prettySCType (SCUInt size) = "sc_dt::sc_uint<" <> pretty size <> ">"

instance Source SCType where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettySCType

prettyFunctionDeclaration :: Annotation ann => FunctionDeclaration ann -> Doc a
prettyFunctionDeclaration FunctionDeclaration {..} =
  prettySCType returnType
    <+> pretty name
      <> prettyArgs
    <+> prettyStatement (Block def body)
  where
    prettyArgs =
      parens . sep . punctuate comma $
        [ prettySCType argType <+> pretty argName
          | (argType, argName) <- args
        ]

instance Annotation ann => Source (FunctionDeclaration ann) where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettyFunctionDeclaration

prettyTranslationUnit :: Annotation ann => TranslationUnit ann -> Doc a
prettyTranslationUnit (TranslationUnit funcs) =
  vsep . punctuate line $ map prettyFunctionDeclaration funcs

instance Annotation ann => Source (TranslationUnit ann) where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettyTranslationUnit
