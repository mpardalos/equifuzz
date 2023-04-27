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
  grownScExpr <- iterateM count (\e -> Hog.choice [castWithDeclaration e, range e]) scExpr
  if isFinalType grownScExpr.annotation
    then return grownScExpr
    else castToFinalType grownScExpr

castWithDeclaration :: SC.Expr SC.BuildOut -> BuildOutM SCConstState (SC.Expr SC.BuildOut)
castWithDeclaration e = do
  varType <- castToType e.annotation
  varIdx <- use (#extraState % #nextVarIdx)
  let varName = "x" <> T.pack (show varIdx)
  #extraState % #nextVarIdx %= (+ 1)
  #extraState % #statements %= (++ [SC.Declaration () varType varName (SC.Cast varType varType e)])
  return (SC.Variable varType varName)

castToFinalType :: SC.Expr SC.BuildOut -> BuildOutM SCConstState (SC.Expr SC.BuildOut)
castToFinalType e = do
  -- TODO Change this to specify the actually allowed "final" types. This works
  -- for now because all of the types in `castToType` are allowed as final
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

isFinalType :: SC.SCType -> Bool
isFinalType SC.SCInt {} = True
isFinalType SC.SCFixed {} = True
isFinalType SC.SCUInt {} = True
isFinalType SC.SCUFixed {} = True
isFinalType SC.CInt = False
isFinalType SC.CUInt = False
isFinalType SC.SCFxnumSubref = False
isFinalType SC.SCIntSubref = False
isFinalType SC.SCUIntSubref = False

range :: SC.Expr SC.BuildOut -> BuildOutM SCConstState (SC.Expr SC.BuildOut)
range e = case (SC.specifiedWidth e.annotation, SC.supportsRange e.annotation) of
  (Just width, Just subrefType) -> do
    hi <- Hog.int (Hog.Range.constant 0 (width - 1))
    lo <- Hog.int (Hog.Range.constant 0 hi)
    return (SC.Range subrefType e hi lo)
  _ -> return e
