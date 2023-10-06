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
import GenSystemC.Reduce
  ( HasReductions (..),
    Reducible (..),
    asReducible,
  )
import GenSystemC.Transformations
  ( BuildOut,
    BuildOutState (headExpr, statements),
    MonadBuild,
    Transformation (..),
    applyTransformation,
    initBuildOutState,
    randomTransformationFor,
    seedExpr,
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
        -- This is only used here, in generateFromProcess. We want this to
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
    finalizeIfNeeded :: MonadBuild m => m ()
    finalizeIfNeeded =
      use (#headExpr % #annotation) >>= \case
        SC.SCInt {} -> pure ()
        SC.SCFixed {} -> pure ()
        SC.SCUInt {} -> pure ()
        SC.SCUFixed {} -> pure ()
        SC.SCBigInt _ -> pure ()
        SC.SCBigUInt _ -> pure ()
        -- Explicitly cast native types
        SC.CInt -> applyTransformation (FunctionalCast SC.CInt)
        SC.CUInt -> applyTransformation (FunctionalCast SC.CUInt)
        SC.CDouble -> applyTransformation (FunctionalCast SC.CDouble)
        SC.CBool -> pure ()
        SC.SCFxnumSubref {width} -> applyTransformation (FunctionalCast (SC.SCUInt width))
        SC.SCIntSubref {width} -> applyTransformation (FunctionalCast (SC.SCUInt width))
        SC.SCUIntSubref {width} -> applyTransformation (FunctionalCast (SC.SCUInt width))
        SC.SCSignedSubref {width} -> applyTransformation (FunctionalCast (SC.SCBigInt width))
        SC.SCUnsignedSubref {width} -> applyTransformation (FunctionalCast (SC.SCBigUInt width))
        SC.SCIntBitref -> applyTransformation (FunctionalCast SC.CBool)
        SC.SCUIntBitref -> applyTransformation (FunctionalCast SC.CBool)
        SC.SCSignedBitref -> applyTransformation (FunctionalCast SC.CBool)
        SC.SCUnsignedBitref -> applyTransformation (FunctionalCast SC.CBool)

data GenerateProcess = GenerateProcess
  { seed :: SC.Expr BuildOut,
    transformations :: [Transformation]
  }

instance HasReductions GenerateProcess where
  mkReductions (GenerateProcess seed transformations) =
    Map.fromSet
      ( \(start, end) ->
          let reducedGenerateProcess = GenerateProcess seed (take start transformations <> drop (end + 1) transformations)
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
