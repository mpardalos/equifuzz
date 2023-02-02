module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Hedgehog.Gen qualified as Hog
import Verismith.Generate (randomMod)
import Verismith.Verilog

boxed :: Text -> Text -> Text
boxed title =
  T.unlines
    . (["┌───" <> title <> "───"] <>)
    . (<> ["└──────"])
    . map ("│ " <>)
    . T.lines

main :: IO ()
main = do
  m :: ModDecl () <- Hog.sample (randomMod 2 4)
  T.putStrLn
    . boxed "Example"
    . genSource
    $ m
