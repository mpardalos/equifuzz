{-# LANGUAGE RecordWildCards #-}

module SystemC where

import Data.Data (Data)
import Data.Text (Text)
import GHC.Generics (Generic)
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

data BinOp = Plus | Minus | Multiply | Divide | BitwiseOr
  deriving (Eq, Show, Generic, Data)

data Expr
  = Constant Int
  | BinOp Expr BinOp Expr
  | Conditional Expr Expr Expr
  | Variable Text
  deriving (Eq, Show, Generic, Data)

data Statement
  = Return Expr
  | Block [Statement]

-- | SystemC types. Constructor parameters correspond to template arguments
data SCType = SCInt Int | SCUInt Int

data FunctionDeclaration = FunctionDeclaration
  { returnType :: SCType,
    name :: Text,
    args :: [(SCType, Text)],
    body :: [Statement]
  }

newtype TranslationUnit = TranslationUnit [FunctionDeclaration]

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

prettyExpr :: Expr -> Doc a
prettyExpr (Constant n) = pretty n
prettyExpr (BinOp l op r) =
  parens . hsep $
    [ prettyExpr l,
      prettyBinOp op,
      prettyExpr r
    ]
prettyExpr (Conditional cond tBranch fBranch) =
  parens . nest 4 . sep $
    [ prettyExpr cond,
      "?" <+> prettyExpr tBranch,
      ":" <+> prettyExpr fBranch
    ]
prettyExpr (Variable name) = pretty name

instance Source Expr where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettyExpr

prettyStatement :: Statement -> Doc a
prettyStatement (Return e) = "return" <+> prettyExpr e <> ";"
prettyStatement (Block statements) =
  vsep ["{", indent 4 . vsep $ prettyStatement <$> statements, "}"]

instance Source Statement where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettyStatement

prettySCType :: SCType -> Doc a
prettySCType (SCInt size) = "sc_int<" <> pretty size <> ">"
prettySCType (SCUInt size) = "sc_uint<" <> pretty size <> ">"

instance Source SCType where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettySCType

prettyFunctionDeclaration :: FunctionDeclaration -> Doc a
prettyFunctionDeclaration FunctionDeclaration {..} =
  prettySCType returnType
    <+> pretty name
      <> prettyArgs
    <+> prettyStatement (Block body)
  where
    prettyArgs =
      parens . sep . punctuate comma $
        [ prettySCType argType <+> pretty argName
          | (argType, argName) <- args
        ]

instance Source FunctionDeclaration where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettyFunctionDeclaration

prettyTranslationUnit :: TranslationUnit -> Doc a
prettyTranslationUnit (TranslationUnit funcs) =
  vsep . punctuate line $ map prettyFunctionDeclaration funcs

instance Source TranslationUnit where
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . prettyTranslationUnit
