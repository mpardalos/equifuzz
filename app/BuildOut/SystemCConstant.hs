{-# LANGUAGE DataKinds #-}
{-# OPTIONS -fdefer-typed-holes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module BuildOut.SystemCConstant where

import BuildOut.Internal (BuildOutM, InputPort)
import BuildOut.SystemC qualified as SC
import Control.Monad.Accum (MonadAccum (add))
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog.Range
import Optics (makePrismLabels)
import SystemC qualified as SC

data SCGenItem
  = SCInput InputPort
  | SCStatement (SC.Statement SC.BuildOut)

makePrismLabels ''SCGenItem

genExpr :: BuildOutM [SCGenItem] (SC.Expr SC.BuildOut)
genExpr = seedExpr >>= grow

seedExpr :: BuildOutM [SCGenItem] (SC.Expr SC.BuildOut)
seedExpr = SC.constant <$> Hog.int (Hog.Range.constant (-128) 128)

grow :: SC.Expr SC.BuildOut -> BuildOutM [SCGenItem] (SC.Expr SC.BuildOut)
grow e = return e

-- castWithDeclaration :: SC.Expr SC.BuildOut -> BuildOutM SCGenItem (SC.Expr SC.BuildOut)
-- castWithDeclaration e = do
--   add [SCStatement _]
--   return _
