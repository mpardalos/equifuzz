module GenSystemC.Config (GenConfig (..)) where

newtype GenConfig = GenConfig
  { growSteps :: Int
  }
