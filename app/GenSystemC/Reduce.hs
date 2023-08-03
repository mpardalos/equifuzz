{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenSystemC.Reduce where

import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)

data Reducible a = Reducible
  { value :: a,
    reductions :: Int -> [Reducible a]
  }
  deriving (Functor, Generic)

class HasReductions a where
  mkReductions :: a -> Int -> [Reducible a]

asReducible :: HasReductions a => a -> Reducible a
asReducible value = Reducible {value, reductions = mkReductions value}

makeFieldLabelsNoPrefix ''Reducible
