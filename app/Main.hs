{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad (forever)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID
import Experiments
import GHC.IO.Encoding
import Hedgehog.Gen qualified as Hog
import Optics
import Shelly qualified as Sh
import Transform
  ( annotateForTransformations,
    randomizeNSP,
    randomizeSP,
  )
import Verismith.Generate as Generate
  ( ConfProperty (..),
    Config (..),
    ProbExpr (..),
    ProbMod (..),
    ProbModItem (..),
    ProbStatement (..),
    Probability (..),
    randomMod, proceduralIO, proceduralSrcIO,
  )
import Verismith.Verilog
import Verismith.Verilog.AST (mainModule, topModuleId)

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
  forever $ do
    m1 <- proceduralSrcIO "mod1" genConfig

    -- putStrLn "---------------"
    -- m2 <- Hog.sample (randomizeSP m1) <&> (#id .~ Identifier "mod2")
    -- equivResult <- runVCFormal (Experiment m1 m2 "vcf_fuzz_equiv")
    -- if equivResult.proofFound
    --   then putStrLn "Proof found. Equivalent modules are equivalent"
    --   else do
    --     uuid <- UUID.nextRandom
    --     putStrLn ("ERROR | No proof found. Equivalent modules are non-equivalent. Saving experiment as " <> show uuid)
    --     Sh.shelly $ do
    --       Sh.mkdir_p "potential_bugs/false_negatives/"
    --       Sh.cp_r "vcf_fuzz_equiv" ("potential_bugs/false_negatives/" <> show uuid)

    putStrLn "---------------"
    m3 <- Hog.sample (randomizeNSP (annotateForTransformations m1)) <&> (topModuleId .~ Identifier "mod2")
    nonEquivResult <- runVCFormal (Experiment m1 m3 "vcf_fuzz_nequiv")
    if not (nonEquivResult.proofFound)
      then putStrLn "No proof found. Non-equivalent modules are non-equivalent"
      else do
        uuid <- UUID.nextRandom
        putStrLn ("ERROR | Proof found. Non-equivalent modules are equivalent. Saving experiment as " <> show uuid)
        Sh.shelly $ do
          Sh.mkdir_p "potential_bugs/false_positives/"
          Sh.cp_r "vcf_fuzz_nequiv" ("potential_bugs/false_positives/" <> show uuid)
