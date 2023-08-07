{-# LANGUAGE DuplicateRecordFields #-}

module GenSystemC
  ( GenConfig (..),
    BuildOut,
    genSystemCConstant,
    generateFromProcess,
    GenerateProcess (..),
    Reducible (..),
  )
where

import Control.Monad.Random.Strict (MonadRandom, Rand, StdGen, foldM)
import Control.Monad.State.Strict (evalStateT, runState)
import Control.Monad.Writer.Strict (MonadWriter, runWriterT, tell)
import Data.Text (Text)
import GenSystemC.GenTransformations
  ( randomTransformationFor,
    seedExpr,
  )
import GenSystemC.Reduce
  ( HasReductions (..),
    Reducible (..),
    asReducible,
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

genSystemCConstant :: GenConfig -> Rand StdGen (Reducible GenerateProcess)
genSystemCConstant cfg = do
  (seed, transformations) <- (`evalStateT` initBuildOutState) . runWriterT $ do
    seed <- seedExpr
    -- We only run the growExpr to output the sta
    _expr <- growExpr seed
    return seed

  return $ asReducible $ GenerateProcess seed transformations
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

generateFromProcess :: Text -> GenerateProcess -> SC.FunctionDeclaration BuildOut
generateFromProcess name GenerateProcess {seed, transformations} =
  let (expr, finalState) =
        foldM (flip applyTransformation) seed transformations
          `runState` initBuildOutState
   in SC.FunctionDeclaration
        { returnType = mapToReturnType expr.annotation,
          name,
          args = [],
          body = finalState.statements ++ [SC.Return () expr]
        }

data GenerateProcess = GenerateProcess
  { seed :: SC.Expr BuildOut,
    transformations :: [Transformation]
  }

instance HasReductions GenerateProcess where
  mkReductions (GenerateProcess seed transformations) window =
    [ Reducible (GenerateProcess seed ts') window (mkReductions (GenerateProcess seed ts'))
      | ts' <- windowsOf window transformations
    ]

  getSize GenerateProcess {transformations} = length transformations

windowsOf :: Int -> [a] -> [[a]]
windowsOf size initLst = take (length initLst) (go initLst)
  where
    go xs = take size (cycle xs) : go (tail (cycle xs))
