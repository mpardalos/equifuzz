module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Verismith.Verilog
import Verismith.Verilog.AST

boxed :: Text -> Text -> Text
boxed title =
  T.unlines
    . (["┌───" <> title <> "───"] <>)
    . (<> ["└──────"])
    . map ("│ " <>)
    . T.lines

main :: IO ()
main = do
  T.putStrLn
    . boxed "Example"
    . genSource
    $ ModDecl @()
      "Example"
      []
      []
      [ Decl Nothing (Port Wire False (Range 0 0) (Identifier "p1")) Nothing
      ]
      []
