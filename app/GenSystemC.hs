{-# LANGUAGE DuplicateRecordFields #-}

module GenSystemC
  ( GenConfig (..),
    BuildOut,
    genSystemCConstant,
    GenerateProcess (..),
  )
where

import Control.Monad.Random.Strict (MonadRandom, Rand, StdGen, foldM)
import Control.Monad.State.Strict (runStateT, runState)
import Control.Monad.Writer.Strict (MonadWriter, runWriterT, tell)
import Data.Text (Text)
import GenSystemC.GenTransformations
  ( isFinalExpr,
    randomFinalTransformation,
    randomTransformationFor,
    seedExpr,
  )
import GenSystemC.Reduce
  ( GenerateProcess (..),
    mkGenerateProcess,
  )
import GenSystemC.Transformations
  ( BuildOut,
    BuildOutState (statements),
    MonadBuildOut,
    Transformation,
    applyTransformation,
    initBuildOutState,
  )
import SystemC qualified as SC
import Util (iterateM)

newtype GenConfig = GenConfig
  { growSteps :: Int
  }

-- | Some systemc types shouldn't be used explicitly at function boundaries.
mapToReturnType :: SC.SCType -> SC.SCType
mapToReturnType SC.SCIntBitref = SC.CBool
mapToReturnType SC.SCUIntBitref = SC.CBool
mapToReturnType SC.SCIntSubref = SC.CInt
mapToReturnType SC.SCUIntSubref = SC.CUInt
mapToReturnType t = t

genSystemCConstant :: GenConfig -> Text -> Rand StdGen (GenerateProcess, SC.FunctionDeclaration BuildOut)
genSystemCConstant cfg name = do
  (seed, state1) <- runStateT seedExpr initBuildOutState
  ((expr, transformations), finalState) <- runStateT (runWriterT (growExpr seed)) state1

  return
    ( mkGenerateProcess seed transformations,
      SC.FunctionDeclaration
        { returnType = mapToReturnType expr.annotation,
          name,
          args = [],
          body = finalState.statements ++ [SC.Return () expr]
        }
    )
  where
    growExpr ::
      (MonadBuildOut m, MonadRandom m, MonadWriter [Transformation] m) =>
      SC.Expr BuildOut ->
      m (SC.Expr BuildOut)
    growExpr =
      iterateM cfg.growSteps $ \e -> do
        transformation <- randomTransformationFor e
        tell [transformation]
        applyTransformation transformation e

data Reducible a = Reducible
  { value :: a,
    reductions :: Int -> [Reducible a]
  }
  deriving Functor

generateProcessToReducible :: Text -> GenerateProcess -> Reducible (SC.FunctionDeclaration BuildOut)
generateProcessToReducible name process =
  Reducible
    { value = generateFromProcess process name,
      reductions = map (generateProcessToReducible name) . process.reductions
    }

generateFromProcess :: GenerateProcess -> Text -> SC.FunctionDeclaration BuildOut
generateFromProcess GenerateProcess {seed, transformations} name =
  let (expr, finalState) =
        foldM (flip applyTransformation) seed transformations
          `runState` initBuildOutState
   in SC.FunctionDeclaration
        { returnType = mapToReturnType expr.annotation,
          name,
          args = [],
          body = finalState.statements ++ [SC.Return () expr]
        }
