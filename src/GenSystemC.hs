{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module GenSystemC (
  -- * Generation
  genSystemC,
  generateFromProcess,
  GenerateProcess (..),

  -- ** Configuration
  OperationsMod,
  GenConfig (..),
)
where

import Control.Monad (replicateM_)
import Control.Monad.Random.Strict (Rand, StdGen)
import Control.Monad.State.Strict (evalStateT, execState)
import Control.Monad.Writer.Strict (MonadWriter (tell), execWriterT)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GenSystemC.Config (GenConfig (..), OperationsMod)
import GenSystemC.Transformations (
  BuildOutState (headExpr, statements),
  MonadBuild,
  Transformation (..),
  applyTransformation,
  initBuildOutState,
  randomTransformationFor,
  seedExpr,
 )
import Optics (Zoom (zoom), use, (%), _1, _2)
import SystemC qualified as SC

genSystemC :: GenConfig -> Rand StdGen GenerateProcess
genSystemC cfg = do
  seed <- seedExpr
  transformations <- execWriterT . flip evalStateT (initBuildOutState seed, []) $
    replicateM_ cfg.growSteps $ do
      e <- use (_1 % #headExpr)
      transformation <- zoom _2 (randomTransformationFor cfg e)
      tell [transformation]
      zoom _1 (applyTransformation cfg transformation)

  return $ GenerateProcess cfg seed transformations

generateFromProcess :: GenConfig -> Text -> GenerateProcess -> SC.FunctionDeclaration
generateFromProcess cfg name GenerateProcess{seed, transformations} =
  let finalState = (`execState` initBuildOutState seed) $ do
        mapM_ (applyTransformation cfg) transformations
        -- This is only used here, in generateFromProcess. We want this to
        -- \*not* be in the GenerateProcess, so that it is dynamically added in
        -- the reduced experiments, depending on what the reduction has left as the final expression.
        finalizeIfNeeded
      body = finalState.statements ++ [SC.Return finalState.headExpr]
      args = nub [(t, v) | (t, v) <- SC.freeVars body, T.length v == 5]
   in SC.FunctionDeclaration
        { returnType = finalState.headExpr.annotation
        , name
        , args
        , body
        }
 where
  finalizeIfNeeded :: MonadBuild m => m ()
  finalizeIfNeeded =
    use (#headExpr % #annotation) >>= \case
      SC.SCInt{} -> pure ()
      SC.SCFixed{} -> pure ()
      SC.SCUInt{} -> pure ()
      SC.SCUFixed{} -> pure ()
      SC.SCBigInt _ -> pure ()
      SC.SCBigUInt _ -> pure ()
      SC.SCLogic -> pure ()
      SC.SCBV{} -> pure ()
      SC.SCLV{} -> pure ()
      -- Explicitly cast native types
      SC.CInt -> applyTransformation cfg (FunctionalCast SC.CInt)
      SC.CUInt -> applyTransformation cfg (FunctionalCast SC.CUInt)
      SC.CDouble -> applyTransformation cfg (FunctionalCast SC.CDouble)
      SC.CBool -> pure ()
      SC.SCFxnumSubref{width} -> applyTransformation cfg (FunctionalCast (SC.SCUInt width))
      SC.SCIntSubref{width} -> applyTransformation cfg (FunctionalCast (SC.SCUInt width))
      SC.SCUIntSubref{width} -> applyTransformation cfg (FunctionalCast (SC.SCUInt width))
      SC.SCSignedSubref{width} -> applyTransformation cfg (FunctionalCast (SC.SCBigInt width))
      SC.SCUnsignedSubref{width} -> applyTransformation cfg (FunctionalCast (SC.SCBigUInt width))
      SC.SCIntBitref -> applyTransformation cfg (FunctionalCast SC.CBool)
      SC.SCUIntBitref -> applyTransformation cfg (FunctionalCast SC.CBool)
      SC.SCSignedBitref -> applyTransformation cfg (FunctionalCast SC.CBool)
      SC.SCUnsignedBitref -> applyTransformation cfg (FunctionalCast SC.CBool)

data GenerateProcess = GenerateProcess
  { cfg :: GenConfig
  , seed :: SC.Expr
  , transformations :: [Transformation]
  }
  deriving (Generic, Show)
