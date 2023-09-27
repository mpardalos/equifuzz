{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module GenSystemC
  ( GenConfig (..),
    BuildOut,
    genSystemCConstant,
    generateFromProcess,
    GenerateProcess (..),
    Reducible (..),
  )
where

import Control.Monad (replicateM_)
import Control.Monad.Random.Strict (Rand, StdGen)
import Control.Monad.State.Strict (evalStateT, execState)
import Control.Monad.Writer.Strict (MonadWriter (tell), runWriterT)
import Data.Map qualified as Map
import Data.Set qualified as Set
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
    BuildOutState (headExpr, statements),
    MonadBuildOut,
    Transformation (..),
    applyTransformation,
    initBuildOutState,
  )
import Optics (use, (%))
import SystemC qualified as SC

newtype GenConfig = GenConfig
  { growSteps :: Int
  }

genSystemCConstant :: GenConfig -> Rand StdGen (Reducible GenerateProcess)
genSystemCConstant cfg = do
  seed <- seedExpr
  ((), transformations) <- runWriterT . (`evalStateT` initBuildOutState seed) $
    replicateM_ cfg.growSteps $ do
      e <- use #headExpr
      transformation <- randomTransformationFor e
      tell [transformation]
      applyTransformation transformation

  return $ asReducible $ GenerateProcess seed transformations

generateFromProcess :: Text -> GenerateProcess -> SC.FunctionDeclaration BuildOut
generateFromProcess name GenerateProcess {seed, transformations} =
  let finalState = (`execState` initBuildOutState seed) $ do
        mapM_ applyTransformation transformations
        -- This is only used here, in generateFromProcess. We don't want this to
        -- \*not* be in the GenerateProcess, so that it is dynamically added in
        -- the reduced experiments, depending on what the reduction has left as the final expression.
        finalizeIfNeeded
   in SC.FunctionDeclaration
        { returnType = finalState.headExpr.annotation,
          name,
          args = [],
          body = finalState.statements ++ [SC.Return () finalState.headExpr]
        }
  where
    finalizeIfNeeded :: MonadBuildOut m => m ()
    finalizeIfNeeded =
      use (#headExpr % #annotation) >>= \case
        SC.SCInt {} -> pure ()
        SC.SCFixed {} -> pure ()
        SC.SCUInt {} -> pure ()
        SC.SCUFixed {} -> pure ()
        -- Explicitly cast native types
        SC.CInt -> applyTransformation (CastWithDeclaration SC.CInt)
        SC.CUInt -> applyTransformation (CastWithDeclaration SC.CUInt)
        SC.CDouble -> applyTransformation (CastWithDeclaration SC.CDouble)
        SC.CBool -> pure ()
        SC.SCFxnumSubref {width} -> applyTransformation (CastWithDeclaration (SC.SCUInt width))
        SC.SCIntSubref {width} -> applyTransformation (CastWithDeclaration (SC.SCUInt width))
        SC.SCUIntSubref {width} -> applyTransformation (CastWithDeclaration (SC.SCUInt width))
        SC.SCIntBitref -> applyTransformation (CastWithDeclaration SC.CBool)
        SC.SCUIntBitref -> applyTransformation (CastWithDeclaration SC.CBool)

data GenerateProcess = GenerateProcess
  { seed :: SC.Expr BuildOut,
    transformations :: [Transformation]
  }

instance HasReductions GenerateProcess where
  mkReductions (GenerateProcess seed transformations) =
    Map.fromSet
      ( \(start, end) ->
          let reducedGenerateProcess = GenerateProcess seed (take start transformations <> drop (end+1) transformations)
           in Reducible
                { value = reducedGenerateProcess,
                  size = length reducedGenerateProcess.transformations,
                  reductions = mkReductions reducedGenerateProcess
                }
      )
      ( Set.fromList
          [ (a, b)
            | a <- [0 .. length transformations - 1],
              b <- [a .. length transformations - 1]
          ]
      )

  getSize GenerateProcess {transformations} = length transformations
