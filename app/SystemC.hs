module SystemC where

import Data.Data (Data)
import Data.Text (Text)
import GHC.Generics (Generic)

data BinOp = Plus | Minus | Multiply | Divide
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

data SCType = SCInt

data FunctionDeclaration = FunctionDeclaration
  { returnType :: SCType,
    name :: Text,
    args :: [(SCType, Text)],
    body :: Statement
  }

newtype TranslationUnit = TranslationUnit [FunctionDeclaration]
