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

import Control.Monad (replicateM_)
import Control.Monad.Random.Strict (Rand, StdGen)
import Control.Monad.State.Strict (evalStateT, execState)
import Control.Monad.Writer.Strict (MonadWriter (tell), runWriterT)
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
    Transformation,
    applyTransformation,
    initBuildOutState,
  )
import Optics (use)
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
  let finalState =
        mapM_ applyTransformation transformations
          `execState` initBuildOutState seed
   in SC.FunctionDeclaration
        { returnType = finalState.headExpr.annotation,
          name,
          args = [],
          body = finalState.statements ++ [SC.Return () finalState.headExpr]
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
