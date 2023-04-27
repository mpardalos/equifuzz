{-# LANGUAGE DataKinds #-}
{-# OPTIONS -fdefer-typed-holes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module BuildOut.SystemCConstant where

import BuildOut.Internal (BuildOutM, iterateM)
import BuildOut.SystemC qualified as SC
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (makePrismLabels, use, (%))
import Optics.State.Operators ((%=))
import SystemC qualified as SC

data SCConstState = SCConstState
  { statements :: [SC.Statement SC.BuildOut],
    nextVarIdx :: Int
  }
  deriving (Generic)

makePrismLabels ''SCConstState

initSCConstState :: SCConstState
initSCConstState =
  SCConstState
    { statements = [],
      nextVarIdx = 0
    }

genExpr :: BuildOutM SCConstState (SC.Expr SC.BuildOut)
genExpr = seedExpr >>= grow

seedExpr :: BuildOutM SCConstState (SC.Expr SC.BuildOut)
seedExpr = SC.constant <$> Hog.int (Hog.Range.constant (-128) 128)

grow :: SC.Expr SC.BuildOut -> BuildOutM SCConstState (SC.Expr SC.BuildOut)
grow scExpr = do
  count <- Hog.int (Hog.Range.linear 1 20)
  iterateM count (\e -> Hog.choice [castWithDeclaration e]) scExpr

castWithDeclaration :: SC.Expr SC.BuildOut -> BuildOutM SCConstState (SC.Expr SC.BuildOut)
castWithDeclaration e = do
  varType <- castToType e.annotation
  varIdx <- use (#extraState % #nextVarIdx)
  let varName = "x" <> T.pack (show varIdx)
  #extraState % #nextVarIdx %= (+ 1)
  #extraState % #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])
  return (SC.Variable varType varName)

-- | Generate a type that the input type can be cast to
castToType :: MonadGen m => SC.SCType -> m SC.SCType
castToType t =
  Hog.choice
    [ SC.SCInt <$> SC.someWidth,
      SC.SCUInt <$> SC.someWidth,
      do
        w <- SC.someWidth
        i <- Hog.int (Hog.Range.constant 0 w)
        return (SC.SCFixed w i)
    ]
