{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildOut (buildOutModules) where

import Control.Applicative (Alternative)
import Control.Monad (guard)
import Control.Monad.Accum
import Control.Monad.State (MonadState (state), StateT (runStateT))
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hedgehog (Gen)
import Hedgehog qualified as Hog
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (view, (%))
import Verismith.Verilog.AST

buildOutModules :: Identifier -> Identifier -> Gen (ModDecl BuildOut, ModDecl BuildOut)
buildOutModules name1 name2 = do
  ((expr1, expr2), ports) <- runBuildOutM (inequivalent >>= grow)
  return
    ( singleExprModule name1 ports expr1,
      singleExprModule name2 ports expr2
    )

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

type ExprPair = (Expr BuildOut, Expr BuildOut)

-- What we really want here is AccumT, but there are no instances for `AccumT w
-- Gen`, so we instead we use StateT (which is isomorphic to AccumT) and provide
-- MonadAccum instance
newtype BuildOutM a = BuildOutM (StateT [Port BuildOut] Gen a)
  deriving newtype (Applicative, Monad, Alternative, Hog.MonadGen)
  deriving stock (Functor)

instance MonadAccum [Port BuildOut] BuildOutM where
  accum f = BuildOutM . state $ \existing ->
    let (val, new) = f existing
     in (val, new <> existing)

runBuildOutM :: BuildOutM a -> Gen (a, [Port BuildOut])
runBuildOutM (BuildOutM m) = runStateT m []

maxWireSize :: Int
maxWireSize = 512

inequivalent :: BuildOutM ExprPair
inequivalent =
  Hog.frequency
    [ (7, differentSignedShift),
      (2, differentInputs),
      (1, differentConstants)
    ]

newPort :: Range BuildOut -> BuildOutM (Port BuildOut)
newPort size = do
  existingPortNames <- map (view (#name % #getIdentifier)) <$> look
  portName <-
    Hog.filterT (not . (`elem` existingPortNames)) $
      Hog.text (Hog.Range.singleton 5) Hog.lower
  let port = Port Wire False size (Identifier portName)
  add [port]
  return port

differentConstants :: BuildOutM ExprPair
differentConstants = do
  n1 <- Hog.int (Hog.Range.constant 0 255)
  n2 <- Hog.int (Hog.Range.constant 0 255)
  guard (n1 /= n2)
  return
    ( Number "differentConstants" (fromIntegral n1),
      Number "differentConstants" (fromIntegral n2)
    )

nonZeroRange :: BuildOutM (Range BuildOut)
nonZeroRange = do
  size <- Hog.int (Hog.Range.linear 1 maxWireSize)
  return (Range (ConstNum () (fromIntegral $ size - 1)) (ConstNum () 0))

differentInputs :: BuildOutM ExprPair
differentInputs = do
  range <- nonZeroRange
  id1 <- view #name <$> newPort range
  id2 <- view #name <$> newPort range
  return
    ( Id "differentInputs" id1,
      Id "differentInputs" id2
    )

-- | Used to make an expression self-determined
concatSingle :: AnnExpr ann -> Expr ann -> Expr ann
concatSingle ann e = Concat ann (e :| [])

differentSignedShift :: BuildOutM ExprPair
differentSignedShift = do
  range <- nonZeroRange
  ident <- view #name <$> newPort range
  return
    ( concatSingle "" (BinOp "differentSignedShift" (Appl "" "$signed" (Id "" ident)) BinASR (Number "" 1)),
      concatSingle "" (BinOp "differentSignedShift" (Appl "" "$unsigned" (Id "" ident)) BinASR (Number "" 1))
    )

-- | Grow both expressions in a pair in an equivalent (but possibly not
-- identical) way
grow :: ExprPair -> BuildOutM ExprPair
grow pair = do
  count <- Hog.int (Hog.Range.linear 1 20)
  iterateM count grow1 pair
  where
    grow1 p =
      Hog.choice
        [ bimapF ifFalse p,
          bimapF ifTrue p,
          bimapF or0 p
        ]

deadExpression :: BuildOutM (Expr BuildOut)
deadExpression = Number "dead" . fromIntegral <$> Hog.int (Hog.Range.constant 1 255)

bimapF :: Applicative f => (a -> f a) -> (a, a) -> f (a, a)
bimapF f (x, y) = (,) <$> f x <*> f y

ifTrue :: Expr BuildOut -> BuildOutM (Expr BuildOut)
ifTrue e = do
  condition <- Number "condT" . fromIntegral <$> Hog.int (Hog.Range.constant 1 255)
  falseBranch <- deadExpression
  return (Cond "ifT" condition e falseBranch)

ifFalse :: Expr BuildOut -> BuildOutM (Expr BuildOut)
ifFalse e = do
  trueBranch <- deadExpression
  return (Cond "ifF" (Number "condF" 0) trueBranch e)

or0 :: Expr BuildOut -> BuildOutM (Expr BuildOut)
or0 e = pure (BinOp "or0" e BinOr (Number "or0" 0))

singleExprModule :: Identifier -> [Port BuildOut] -> Expr BuildOut -> ModDecl BuildOut
singleExprModule name inPorts e =
  ModDecl
    { annotation = (),
      params = [],
      id = name,
      inPorts,
      outPorts = [outPort],
      items = portDecls ++ [ModCA () (ContAssign "y" e)]
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
          name = "y"
        }

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ x = pure x
iterateM 1 f x = f x
iterateM n f x = do
  x' <- f x
  iterateM (n - 1) f x'
