{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SystemC where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Optics (A_Getter, LabelOptic, makeFieldLabelsNoPrefix, to)
import Optics.Label (LabelOptic (labelOptic))
import Prettyprinter (
  Doc,
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

class Source a where
  genSource :: a -> Text
  default genSource :: Pretty a => a -> Text
  genSource =
    renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty

data BinOp
  = Plus
  | Minus
  | Multiply
  | Divide
  | BitwiseOr
  | LogicalOr
  | LogicalAnd
  | Assign
  | PlusAssign
  | MinusAssign
  | TimesAssign
  | DivideAssign
  | ModAssign
  | BitwiseAndAssign
  | BitwiseOrAssign
  | BitwiseXorAssign
  | LeftShiftAssign
  | RightShiftAssign
  | Equals
  deriving (Eq, Show, Generic, Data, Ord, Bounded, Enum)

data UnaryOp
  = PreIncrement
  | PreDecrement
  | PostIncrement
  | PostDecrement
  | LogicalNot
  deriving (Eq, Show, Generic, Data, Ord, Bounded, Enum)

type ExprAnn = SCType

data Expr
  = -- Some text, used as-is. Useful for hex/octal/binary constants
    Literal ExprAnn Text
  | Constant ExprAnn Integer
  | BinOp ExprAnn Expr BinOp Expr
  | UnaryOp ExprAnn UnaryOp Expr
  | Conditional ExprAnn Expr Expr Expr
  | Variable ExprAnn Text
  | Cast ExprAnn SCType Expr
  | Bitref ExprAnn Expr Integer
  | MethodCall ExprAnn Expr Text [Expr]
  | FunctionCall ExprAnn Expr [Expr]
  deriving (Generic, Eq, Ord, Show, Data)

-- | Extract all variables referenced by the expression
freeVars :: Data from => from -> [(SCType, Text)]
freeVars e = [(t, name) | Variable t name <- universeBi e]

instance HasField "annotation" Expr ExprAnn where
  getField (Literal ann _) = ann
  getField (Constant ann _) = ann
  getField (BinOp ann _ _ _) = ann
  getField (UnaryOp ann _ _) = ann
  getField (Conditional ann _ _ _) = ann
  getField (Variable ann _) = ann
  getField (Cast ann _ _) = ann
  getField (Bitref ann _ _) = ann
  getField (MethodCall ann _ _ _) = ann
  getField (FunctionCall ann _ _) = ann

instance LabelOptic "annotation" A_Getter Expr Expr ExprAnn ExprAnn where
  labelOptic = to (getField @"annotation")

type VarName = Text

data Statement
  = Return Expr
  | AssignmentDeclaration SCType VarName Expr
  | Declaration SCType VarName
  | Assignment VarName Expr
  | If Expr Statement (Maybe Statement)
  | Block [Statement]
  deriving (Generic, Eq, Ord, Show, Data)

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
  | SCFxnumBitref
  | SCLogic
  | SCBV {width :: Int}
  | SCLV {width :: Int}
  | CUInt
  | CInt
  | CDouble
  | CBool
  deriving (Eq, Show, Ord, Generic, Data)

-- | Methods on SystemC types (@.and_reduce()@, @.or_reduce()@, etc.)  These are
-- assumed to have a fixed return type, across different SC types.  If we add
-- methods whose return type differs between SystemC types, this will need to be
-- changed
data SCMethod
  = ReduceAnd
  | ReduceNand
  | ReduceOr
  | ReduceNor
  | ReduceXor
  | ReduceXNor
  | ToInt
  | ToUInt
  | ToLong
  | ToULong
  | ToInt64
  | ToUInt64
  | ToDouble
  | Value
  | ToBool
  | Is01
  | Reverse
  | Length
  deriving (Eq, Show, Ord, Generic, Data, Enum, Bounded)

allReductions :: Map SCMethod SCType
allReductions =
  [ (ReduceAnd, CBool)
  , (ReduceNand, CBool)
  , (ReduceOr, CBool)
  , (ReduceNor, CBool)
  , (ReduceXor, CBool)
  , (ReduceXNor, CBool)
  ]

integerExplicitConversions :: Map SCMethod SCType
integerExplicitConversions =
  [ (ToInt, CInt)
  , (ToUInt, CUInt)
  , -- Lies, but close enough
    (ToLong, CInt)
  , (ToULong, CUInt)
  , (ToInt64, CInt)
  , (ToUInt64, CUInt)
  ]

floatingExplicitConversions :: Map SCMethod SCType
floatingExplicitConversions =
  [ (ToDouble, CDouble)
  ]

allExplicitConversions :: Map SCMethod SCType
allExplicitConversions = integerExplicitConversions <> floatingExplicitConversions

lengthMethod :: Map SCMethod SCType
lengthMethod = [(Length, CInt)]

noTypes :: SCType -> Bool
noTypes = const False

allTypes :: SCType -> Bool
allTypes = const True

allCNumericTypes :: SCType -> Bool
allCNumericTypes CUInt{} = True
allCNumericTypes CInt{} = True
allCNumericTypes CDouble{} = True
allCNumericTypes CBool{} = True
allCNumericTypes _ = False

allNumericTypes :: SCType -> Bool
allNumericTypes SCInt{} = True
allNumericTypes SCUInt{} = True
allNumericTypes SCBigInt{} = True
allNumericTypes SCBigUInt{} = True
allNumericTypes SCFixed{} = True
allNumericTypes SCUFixed{} = True
allNumericTypes SCFxnumSubref{} = True
allNumericTypes SCIntSubref{} = True
allNumericTypes SCUIntSubref{} = True
allNumericTypes SCSignedSubref{} = True
allNumericTypes SCUnsignedSubref{} = True
allNumericTypes SCIntBitref{} = True
allNumericTypes SCUIntBitref{} = True
allNumericTypes SCSignedBitref{} = True
allNumericTypes SCUnsignedBitref{} = True
allNumericTypes SCFxnumBitref{} = True
allNumericTypes SCLogic{} = False
allNumericTypes SCBV{} = False
allNumericTypes SCLV{} = False
allNumericTypes CUInt{} = True
allNumericTypes CInt{} = True
allNumericTypes CDouble{} = True
allNumericTypes CBool{} = True

allSCNumericTypes :: SCType -> Bool
allSCNumericTypes CUInt = False
allSCNumericTypes CInt = False
allSCNumericTypes CDouble = False
allSCNumericTypes CBool = False
allSCNumericTypes t = allNumericTypes t

-- | Possible operations on a SystemC type
data Operations = Operations
  { methods :: Map SCMethod SCType
  -- ^ Available methods and their result type
  , bitSelect :: Maybe SCType
  -- ^ Result of the bit select operator (@x[10]@), if that is available
  , partSelect :: Maybe (Int -> SCType)
  -- ^ Result of the part select operator (@x(0, 10)@), if that is available
  , unaryOperators :: [UnaryOp]
  , assignFrom :: SCType -> Bool
  , constructFrom :: SCType -> Bool
  , implicitCasts :: [SCType]
  }
  deriving (Generic)

methodName :: SCMethod -> Text
methodName ReduceAnd = "and_reduce"
methodName ReduceNand = "nand_reduce"
methodName ReduceOr = "or_reduce"
methodName ReduceNor = "nor_reduce"
methodName ReduceXor = "xor_reduce"
methodName ReduceXNor = "xnor_reduce"
methodName ToInt = "to_int"
methodName ToUInt = "to_uint"
methodName ToLong = "to_long"
methodName ToULong = "to_ulong"
methodName ToInt64 = "to_int64"
methodName ToUInt64 = "to_uint64"
methodName ToDouble = "to_double"
methodName Value = "value"
methodName ToBool = "to_bool"
methodName Is01 = "is_01"
methodName Reverse = "reverse"
methodName Length = "length"

isLValue :: Expr -> Bool
isLValue Variable{} = True
isLValue UnaryOp{} = True
isLValue (BinOp _ _ op _) =
  elem @[]
    op
    [ PlusAssign
    , MinusAssign
    , TimesAssign
    , DivideAssign
    , ModAssign
    , BitwiseAndAssign
    , BitwiseOrAssign
    , BitwiseXorAssign
    , LeftShiftAssign
    , RightShiftAssign
    ]
isLValue _ = False

allUnaryOperators :: [UnaryOp]
allUnaryOperators = [minBound .. maxBound]

-- | Get all possible operations for a SystemC expression
operations :: SCType -> Operations
operations t =
  case t of
    SCInt{} ->
      Operations
        { bitSelect = Just SCIntBitref
        , partSelect = Just SCIntSubref
        , implicitCasts = [CInt]
        , methods =
            allReductions
              <> allExplicitConversions
              <> lengthMethod
        , unaryOperators = []
        , constructFrom = allNumericTypes
        , assignFrom = allNumericTypes
        }
    SCUInt{} ->
      Operations
        { bitSelect = Just SCUIntBitref
        , partSelect = Just SCUIntSubref
        , implicitCasts = [CUInt]
        , methods =
            allReductions
              <> allExplicitConversions
              <> lengthMethod
        , unaryOperators = []
        , constructFrom = allNumericTypes
        , assignFrom = allNumericTypes
        }
    SCIntBitref ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [CInt] -- Actually uint64
        , methods = lengthMethod
        , -- Missing operator! and operator~
          unaryOperators = []
        , constructFrom = noTypes
        , assignFrom = \case
            -- Only if it is an lvalue
            CBool -> True
            SCIntBitref{} -> True
            _ -> False
        }
    SCUIntBitref ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [CBool]
        , methods = []
        , unaryOperators = []
        , constructFrom = noTypes
        , assignFrom = \case
            -- Only if it is an lvalue
            CBool -> True
            SCIntBitref{} -> True
            _ -> False
        }
    SCIntSubref{} ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [CUInt]
        , methods =
            allReductions
              <> allExplicitConversions
              <> lengthMethod
        , unaryOperators = []
        , constructFrom = noTypes
        , assignFrom = noTypes
        }
    SCUIntSubref{} ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [CUInt]
        , methods =
            allReductions
              <> allExplicitConversions
              <> lengthMethod
        , unaryOperators = []
        , constructFrom = noTypes
        , assignFrom = noTypes
        }
    SCBigInt{} ->
      Operations
        { bitSelect = Just SCSignedBitref
        , partSelect = Just SCSignedSubref
        , implicitCasts = []
        , methods =
            allReductions
              <> allExplicitConversions
              <> lengthMethod
        , unaryOperators = []
        , constructFrom = allNumericTypes
        , assignFrom = allNumericTypes
        }
    SCBigUInt{} ->
      Operations
        { bitSelect = Just SCUnsignedBitref
        , partSelect = Just SCUnsignedSubref
        , implicitCasts = []
        , methods =
            allReductions
              <> allExplicitConversions
              <> lengthMethod
        , unaryOperators = []
        , constructFrom = allNumericTypes
        , assignFrom = allNumericTypes
        }
    SCSignedBitref ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [CBool]
        , -- Missing operator! and operator~
          methods = lengthMethod <> [(ToBool, CBool)]
        , unaryOperators = []
        , constructFrom = noTypes
        , assignFrom = noTypes
        }
    SCUnsignedBitref ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [CBool]
        , -- Missing operator! and operator~
          methods = lengthMethod <> [(ToBool, CBool)]
        , unaryOperators = []
        , constructFrom = noTypes
        , assignFrom = noTypes
        }
    SCSignedSubref{width} ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [SCBigUInt width]
        , methods = allReductions <> allExplicitConversions
        , unaryOperators = []
        , constructFrom = noTypes
        , assignFrom = noTypes
        }
    SCUnsignedSubref{width} ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [SCBigUInt width]
        , methods = allReductions <> allExplicitConversions
        , unaryOperators = []
        , constructFrom = noTypes
        , assignFrom = noTypes
        }
    SCLogic ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = []
        , methods =
            [ (ToBool, CBool)
            , (Is01, CBool)
            -- , (Value, SCLogicValueT) -- We don't have sc_logic_value_t
            ]
        , unaryOperators = []
        , constructFrom = \case
            CBool -> True
            -- Cast from long is ambiguous, so we ban int altogether
            _ -> False
        , assignFrom = noTypes
        }
    SCBV{} ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = []
        , methods =
            -- Missing lrotate, rrotate because they take arguments
            allReductions
              <> integerExplicitConversions
              <> lengthMethod
              <> [ (Is01, CBool)
                 , (Reverse, t)
                 ]
        , unaryOperators = []
        , -- These Constructors/assignments are ambiguous
          constructFrom = \case
            SCFixed{} -> False
            SCUFixed{} -> False
            CDouble -> False
            _ -> True
        , assignFrom = \case
            SCFixed{} -> False
            SCUFixed{} -> False
            CDouble -> False
            _ -> True
        }
    SCLV{} ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = []
        , methods =
            -- Missing lrotate, rrotate because they take arguments
            allReductions
              <> integerExplicitConversions
              <> lengthMethod
              <> [ (Is01, CBool)
                 , (Reverse, t)
                 ]
        , unaryOperators = []
        , -- Construction/assignments from sc_(u)fixed are ambiguous
          constructFrom = \case
            SCFixed{} -> False
            SCUFixed{} -> False
            CDouble -> False
            _ -> True
        , assignFrom = \case
            SCFixed{} -> False
            SCUFixed{} -> False
            CDouble -> False
            _ -> True
        }
    SCFixed{} ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [CDouble]
        , methods =
            allExplicitConversions
        , unaryOperators = []
        , constructFrom = allNumericTypes
        , assignFrom = allNumericTypes
        }
    SCUFixed{} ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [CDouble]
        , methods = []
        , unaryOperators = []
        , constructFrom = allNumericTypes
        , assignFrom = allNumericTypes
        }
    SCFxnumBitref{} ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [CBool]
        , methods = []
        , unaryOperators = []
        , constructFrom = noTypes
        , assignFrom = \case
            CBool -> True
            _ -> False
        }
    SCFxnumSubref{width} ->
      Operations
        { bitSelect = Nothing
        , partSelect = Nothing
        , implicitCasts = [SCBV width]
        , methods =
            allReductions
              <> allExplicitConversions
              <> lengthMethod
        , unaryOperators = []
        , constructFrom = noTypes
        , assignFrom = \case
            CInt -> True
            CUInt -> True
            SCInt{} -> True
            SCUInt{} -> True
            SCBigInt{} -> True
            SCBigUInt{} -> True
            SCBV{} -> True
            SCLV{} -> True
            _ -> False
        }
    CUInt -> cTypeOperations
    CInt -> cTypeOperations
    CDouble -> cTypeOperations
    CBool -> cTypeOperations

cTypeOperations :: Operations
cTypeOperations =
  Operations
    { bitSelect = Nothing
    , partSelect = Nothing
    , implicitCasts = cTypes
    , methods = []
    , unaryOperators = []
    , constructFrom = (`elem` cTypes)
    , assignFrom = (`elem` cTypes)
    }
 where
  cTypes :: [SCType]
  cTypes = [CUInt, CInt, CDouble, CBool]

-- | Give the bitwidth of the type where that exists (i.e. SystemC types with a
-- width template parameters)
knownWidth :: SCType -> Maybe Int
knownWidth (SCInt n) = Just n
knownWidth (SCBigInt n) = Just n
knownWidth SCFixed{w} = Just w
knownWidth (SCUInt n) = Just n
knownWidth (SCBigUInt n) = Just n
knownWidth SCUFixed{w} = Just w
knownWidth SCFxnumSubref{width} = Just width
knownWidth SCIntSubref{width} = Just width
knownWidth SCUIntSubref{width} = Just width
knownWidth SCSignedSubref{width} = Just width
knownWidth SCUnsignedSubref{width} = Just width
knownWidth SCFxnumBitref = Nothing
knownWidth SCIntBitref = Nothing
knownWidth SCUIntBitref = Nothing
knownWidth SCSignedBitref = Nothing
knownWidth SCUnsignedBitref = Nothing
knownWidth SCLogic = Nothing
knownWidth SCBV{width} = Just width
knownWidth SCLV{width} = Just width
knownWidth CInt = Nothing
knownWidth CUInt = Nothing
knownWidth CDouble = Nothing
knownWidth CBool = Nothing

data Signature = Signature
  { returnType :: SCType
  , name :: Text
  , args :: [(SCType, Text)]
  }
  deriving (Generic, Eq, Ord, Show, Data)


data FunctionDeclaration = FunctionDeclaration
  { sig :: Signature
  , body :: [Statement]
  }
  deriving (Generic, Eq, Ord, Show, Data)

newtype TranslationUnit = TranslationUnit [FunctionDeclaration]
  deriving (Generic, Eq, Show, Data)

-- | This needs to be included in the final program
includeHeader :: Text
includeHeader = "#define SC_INCLUDE_FX\n#include <systemc>"

instance Pretty BinOp where
  pretty Plus = "+"
  pretty Minus = "-"
  pretty Multiply = "*"
  pretty Divide = "/"
  pretty BitwiseOr = "|"
  pretty LogicalOr = "||"
  pretty LogicalAnd = "&&"
  pretty Assign = "="
  pretty PlusAssign = "+="
  pretty MinusAssign = "-="
  pretty TimesAssign = "*="
  pretty DivideAssign = "/="
  pretty ModAssign = "%="
  pretty BitwiseAndAssign = "&="
  pretty BitwiseOrAssign = "|="
  pretty BitwiseXorAssign = "^="
  pretty LeftShiftAssign = "<<="
  pretty RightShiftAssign = ">>="
  pretty Equals = "=="

instance Source BinOp

instance Pretty SCMethod where
  pretty = pretty . methodName

annComment :: Doc a -> Doc a
annComment ann = "/* " <> ann <> " */"

instance Pretty Expr where
  pretty (Literal _ txt) =
    pretty txt
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
      LogicalNot -> "!" <> pretty expr
  pretty (Conditional _ cond tBranch fBranch) =
    "("
      <> ( align . vsep $
            [ pretty cond
            , "?" <+> pretty tBranch
            , ":" <+> pretty fBranch
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
  pretty (FunctionCall _ e args) =
    pretty e
      <> "("
      <> hsep (punctuate ", " (map pretty args))
      <> ")"
  pretty (Bitref _ e bit) = pretty e <> "[" <> pretty bit <> "]"

instance Source Expr

prettyBlock :: [Statement] -> Doc a
prettyBlock statements = vsep ["{", indent 4 . vsep $ pretty <$> statements, "}"]

instance Pretty Statement where
  pretty (Return e) =
    "return" <+> pretty e <> ";"
  pretty (AssignmentDeclaration t name expr) =
    pretty t <+> pretty name <+> "=" <+> pretty expr <> ";"
  pretty (Assignment name expr) =
    pretty name <+> "=" <+> pretty expr <> ";"
  pretty (Declaration t name) =
    pretty t <+> pretty name <> ";"
  pretty (If cond ifT Nothing) =
    "if (" <> pretty cond <> ") " <> pretty ifT
  pretty (If cond ifT (Just ifF)) =
    "if ("
      <> pretty cond
      <> ") "
      <> pretty ifT
      <> " else "
      <> pretty ifF
  pretty (Block statements) =
    prettyBlock statements

instance Source Statement

instance Pretty SCType where
  pretty (SCInt size) = "sc_dt::sc_int<" <> pretty size <> ">"
  pretty (SCUInt size) = "sc_dt::sc_uint<" <> pretty size <> ">"
  pretty (SCBigInt size) = "sc_dt::sc_bigint<" <> pretty size <> ">"
  pretty (SCBigUInt size) = "sc_dt::sc_biguint<" <> pretty size <> ">"
  pretty (SCFixed w i) = "sc_dt::sc_fixed<" <> pretty w <> "," <> pretty i <> ">"
  pretty (SCUFixed w i) = "sc_dt::sc_ufixed<" <> pretty w <> "," <> pretty i <> ">"
  pretty SCFxnumBitref{} = "sc_dt::sc_fxnum_bitref"
  pretty SCFxnumSubref{} = "sc_dt::sc_fxnum_subref"
  pretty SCIntSubref{} = "sc_dt::sc_int_subref"
  pretty SCUIntSubref{} = "sc_dt::sc_uint_subref"
  pretty SCSignedSubref{} = "sc_dt::sc_signed_subref"
  pretty SCUnsignedSubref{} = "sc_dt::sc_unsigned_subref"
  pretty SCIntBitref = "sc_dt::sc_int_bitref"
  pretty SCUIntBitref = "sc_dt::sc_uint_bitref"
  pretty SCSignedBitref = "sc_dt::sc_signed_bitref"
  pretty SCUnsignedBitref = "sc_dt::sc_unsigned_bitref"
  pretty SCBV{width} = "sc_dt::sc_bv<" <> pretty width <> ">"
  pretty SCLV{width} = "sc_dt::sc_lv<" <> pretty width <> ">"
  pretty SCLogic = "sc_dt::sc_logic"
  pretty CInt = "int"
  pretty CUInt = "unsigned"
  pretty CDouble = "double"
  pretty CBool = "bool"

instance Source SCType

instance Pretty FunctionDeclaration where
  pretty FunctionDeclaration{..} =
    pretty sig.returnType
      <+> pretty sig.name
      <> prettyArgs
      <+> prettyBlock body
   where
    prettyArgs =
      parens . sep . punctuate comma $
        [ pretty argType <+> pretty argName
        | (argType, argName) <- sig.args
        ]

instance Source FunctionDeclaration

instance Pretty TranslationUnit where
  pretty (TranslationUnit funcs) =
    vsep . punctuate line $ map pretty funcs

instance Source TranslationUnit

makeFieldLabelsNoPrefix 'Operations
