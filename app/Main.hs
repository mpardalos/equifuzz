{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.IO.Encoding
import Hedgehog.Gen qualified as Hog
import Optics
import Experiments
import Transform
  ( annotateForTransformations, randomizeSP, randomizeNSP,
  )
import Verismith.Generate as Generate
  ( ConfProperty (..),
    Config (..),
    ProbExpr (..),
    ProbMod (..),
    ProbModItem (..),
    ProbStatement (..),
    Probability (..),
    randomMod,
  )
import Verismith.Verilog

boxed :: Text -> Text -> Text
boxed title =
  T.unlines
    . (["╭───" <> title <> "───"] <>)
    . (<> ["╰──────"])
    . map ("│ " <>)
    . T.lines

genConfig :: Generate.Config
genConfig =
  Config
    { probability =
        Probability
          { modItem =
              ProbModItem
                { assign = 2,
                  seqAlways = 0,
                  combAlways = 1,
                  inst = 0
                },
            stmnt =
              ProbStatement
                { block = 0,
                  nonBlock = 3,
                  cond = 1,
                  for = 0
                },
            expr =
              ProbExpr
                { num = 1,
                  id = 5,
                  rangeSelect = 5,
                  unOp = 5,
                  binOp = 5,
                  cond = 5,
                  concat = 3,
                  str = 0,
                  signed = 1,
                  unsigned = 5
                },
            mod =
              ProbMod
                { dropOutput = 0,
                  keepOutput = 1
                }
          },
      property =
        ConfProperty
          { size = 50,
            seed = Nothing,
            stmntDepth = 3,
            modDepth = 2,
            maxModules = 5,
            sampleMethod = "random",
            sampleSize = 10,
            combine = False,
            nonDeterminism = 0,
            determinism = 1,
            defaultYosys = Nothing
          }
    }

main :: IO ()
main = do
  setLocaleEncoding utf8
  m1 :: ModDecl () <- Hog.sample (randomMod 2 4) <&> (#id .~ Identifier "mod1")
  m2 <- Hog.sample (randomizeSP m1) <&> (#id .~ Identifier "mod2")
  m3 <- Hog.sample (randomizeNSP (annotateForTransformations m1)) <&> (#id .~ Identifier "mod2")

  equivResult <- runVCFormal (Experiment m1 m2 "vcf_fuzz_equiv")
  if equivResult.proofFound
    then putStrLn "Proof found. Equivalent modules are equivalent"
    else putStrLn "ERROR | No proof found. Equivalent modules are non-equivalent"

  putStrLn "---------------"

  nonEquivResult <- runVCFormal (Experiment m1 m3 "vcf_fuzz_nequiv")
  if nonEquivResult.proofFound
    then putStrLn "ERROR | Proof found. Non-equivalent modules are equivalent"
    else putStrLn "No proof found. Non-equivalent modules are non-equivalent"
