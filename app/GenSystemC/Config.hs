{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenSystemC.Config
  ( GenConfig (..),
    TransformationsF (..),
    TransformationsConfig,
    allTransformationsEnabled
  )
where

import GHC.Generics (Generic)
import GenSystemC.Transformations (BuildOut)
import Optics
import SystemC qualified as SC

data TransformationsF a = TransformationsF
  { castWithDeclaration :: a,
    range :: a,
    arithmetic :: a,
    useAsCondition :: a,
    bitSelect :: a,
    applyReduction :: a
  }
  deriving (Functor, Foldable, Traversable)

instance Applicative TransformationsF where
  pure x = TransformationsF x x x x x x
  (<*>)
    (TransformationsF f1 f2 f3 f4 f5 f6)
    (TransformationsF x1 x2 x3 x4 x5 x6) =
      TransformationsF
        (f1 x1)
        (f2 x2)
        (f3 x3)
        (f4 x4)
        (f5 x5)
        (f6 x6)

-- | Additional conditions for each transformation. Use `const False` to
-- completely disable a transformation or `const True` to enabled it with no
-- additional constraints
type TransformationsConfig = TransformationsF (SC.Expr BuildOut -> Bool)

allTransformationsEnabled :: TransformationsConfig
allTransformationsEnabled = pure (const True)

data GenConfig = GenConfig
  { growSteps :: Int,
    transformationsConfig :: TransformationsConfig
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''GenConfig
makeFieldLabelsNoPrefix ''TransformationsF
