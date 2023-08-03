{-# LANGUAGE DuplicateRecordFields #-}

module GenSystemC.Reduce where

data Reducible a = Reducible
  { value :: a,
    reductions :: Int -> [Reducible a]
  }
  deriving (Functor)

class HasReductions a where
  mkReductions :: a -> Int -> [Reducible a]

asReducible :: HasReductions a => a -> Reducible a
asReducible value = Reducible {value, reductions = mkReductions value}
