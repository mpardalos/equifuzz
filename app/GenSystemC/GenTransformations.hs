{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module GenSystemC.GenTransformations (seedExpr, randomTransformationFor) where

import Control.Monad (guard, join)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform, weighted)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import GenSystemC.Transformations
  ( BuildOut,
    Transformation (..),
  )
import SystemC qualified as SC

randomTransformationFor :: forall m. MonadRandom m => SC.Expr BuildOut -> m Transformation
randomTransformationFor e =
  join . weighted . map (,1) . catMaybes $
    [ castWithDeclaration
    , range
#ifndef EVALUATION_VERSION
    , arithmetic
    , useAsCondition
    , bitSelect
    , applyReduction
#endif
    ]
  where
    castWithDeclaration :: Maybe (m Transformation)
    castWithDeclaration = Just (CastWithDeclaration <$> castTargetType e.annotation)

    range :: Maybe (m Transformation)
    range = do
      guard (e.annotation `SC.supports` SC.PartSelect)
      exprWidth <- SC.knownWidth e.annotation
      return $ do
        hi <- getRandomR (0, exprWidth - 1)
        lo <- getRandomR (0, hi)
        return (Range hi lo)

    arithmeticResultType :: Maybe SC.SCType
    arithmeticResultType
      | [t] <-
          Set.toList $
            Set.intersection
              (Set.fromList [SC.CInt, SC.CUInt, SC.CDouble])
              (SC.implicitCastTargetsOf e.annotation) =
          Just t
      | otherwise = Nothing

    arithmetic :: Maybe (m Transformation)
    arithmetic = do
      resultType <- arithmeticResultType
      return $ do
        op <- uniform [SC.Plus, SC.Minus, SC.Multiply]
        constant <- someConstant resultType
        return (Arithmetic op constant)

    useAsCondition :: Maybe (m Transformation)
    useAsCondition = do
      guard (SC.CBool `elem` SC.implicitCastTargetsOf e.annotation)
      return
        ( UseAsCondition
            <$> someConstant SC.CInt
            <*> someConstant SC.CInt
        )

    bitSelect :: Maybe (m Transformation)
    bitSelect = do
      guard (e.annotation `SC.supports` SC.BitSelect)
      width <- SC.knownWidth e.annotation
      return (BitSelect <$> getRandomR (0, width - 1))

    applyReduction :: Maybe (m Transformation)
    applyReduction = do
      let options =
            [ ApplyReduction op
              | SC.ReductionOperation op <-
                  Set.elems $ SC.supportedOperations e.annotation
            ]
      guard (not . null $ options)
      return (uniform options)

    someConstant :: SC.SCType -> m (SC.Expr BuildOut)
    someConstant t = SC.Constant t <$> getRandomR (-1024, 1024)

seedExpr :: MonadRandom m => m (SC.Expr BuildOut)
seedExpr = do
  value <- getRandomR (-128, 128)
  return (SC.Constant SC.CInt value)

-- Generate a type that the input type can be cast to
castTargetType :: MonadRandom m => SC.SCType -> m SC.SCType
castTargetType = \case
  SC.SCFxnumSubref {} -> join $ uniform [someInt, someUInt]
  _ -> join $ uniform [someInt, someUInt, someFixed, someUFixed]
  where
    someInt = SC.SCInt <$> someWidth
    someUInt = SC.SCUInt <$> someWidth
    someFixed = do
      w <- someWidth
      i <- getRandomR (0, w)
      return (SC.SCFixed w i)
    someUFixed = do
      w <- someWidth
      i <- getRandomR (0, w)
      return (SC.SCUFixed w i)

    someWidth :: MonadRandom m => m Int
    someWidth = getRandomR (1, 64)
