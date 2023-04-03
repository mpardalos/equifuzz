{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildOut.Verilog where

import BuildOut.Internal
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Verismith.Verilog.AST

newtype ExprSource = ExprSource Text
  deriving newtype (IsString)
  deriving stock (Generic, Data)

instance Show ExprSource where
  show (ExprSource txt) = T.unpack txt

data BuildOut

instance Annotation BuildOut where
  type AnnExpr BuildOut = ExprSource
  type AnnConstExpr BuildOut = ()
  type AnnStatement BuildOut = ()
  type AnnModItem BuildOut = ()
  type AnnModDecl BuildOut = ()

instance Default ExprSource where
  def = ExprSource ""

newPort :: Int -> BuildOutM s (Port BuildOut)
newPort size = do
  InputPort _ name <- newInputPort size
  return (Port Wire False (rangeFromSize (fromIntegral size)) (Identifier name))

-- | Used to make an expression self-determined
concatSingle :: AnnExpr ann -> Expr ann -> Expr ann
concatSingle ann e = Concat ann (e :| [])

deadExpression :: BuildOutM s (Expr BuildOut)
deadExpression = Number "dead" . fromIntegral <$> Hog.int (Hog.Range.constant (-255) 255)

ifTrue :: Expr BuildOut -> BuildOutM s (Expr BuildOut)
ifTrue e = do
  condition <- Number "condT" . fromIntegral <$> Hog.int (Hog.Range.constant 1 255)
  falseBranch <- deadExpression
  return (Cond "ifT" condition e falseBranch)

ifFalse :: Expr BuildOut -> BuildOutM s (Expr BuildOut)
ifFalse e = do
  trueBranch <- deadExpression
  return (Cond "ifF" (Number "condF" 0) trueBranch e)

or0 :: Expr BuildOut -> BuildOutM s (Expr BuildOut)
or0 e = pure (BinOp "or0" e BinOr (Number "or0" 0))

signedUnsigned :: Expr BuildOut -> Expr BuildOut
signedUnsigned e = Appl "signedUnsigned" "$signed" (Appl "signedUnsigned" "$unsigned" e)

unsignedSigned :: Expr BuildOut -> Expr BuildOut
unsignedSigned e = Appl "unsignedSigned" "$unsigned" (Appl "unsignedSigned" "$signed" e)

singleExprModule :: Identifier -> [Port BuildOut] -> Expr BuildOut -> ModDecl BuildOut
singleExprModule name inPorts e =
  ModDecl
    { annotation = (),
      params = [],
      id = name,
      inPorts,
      outPorts = [outPort],
      items = portDecls ++ [ModCA () (ContAssign "out" e)]
    }
  where
    portDecls =
      Decl () (Just PortOut) outPort Nothing
        : [ Decl () (Just PortIn) port Nothing
            | port <- inPorts
          ]
    outPort =
      Port
        { portType = Wire,
          signed = False,
          size = Range (ConstNum () (fromIntegral $ maxWireSize - 1)) (ConstNum () 0),
          name = "out"
        }
