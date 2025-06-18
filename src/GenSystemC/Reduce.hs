{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenSystemC.Reduce where

import Data.Map (Map)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)

data Reducible a = Reducible
  { value :: a
  , size :: Int
  , reductions :: Map (Int, Int) (Reducible a)
  }
  deriving (Functor, Generic)

class HasReductions a where
  mkReductions :: a -> Map (Int, Int) (Reducible a)
  getSize :: a -> Int

asReducible :: HasReductions a => a -> Reducible a
asReducible value = Reducible{value, size = getSize value, reductions = mkReductions value}

makeFieldLabelsNoPrefix ''Reducible
