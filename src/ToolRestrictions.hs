{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ToolRestrictions where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import GenSystemC (GenMods (..), Transformation (..))
import SystemC qualified as SC

vcfMods :: GenMods
vcfMods = GenMods{name = "vcf", transformationAllowed}
 where
  transformationAllowed e t =
    case (e.annotation, t) of
      (SC.CDouble, FunctionalCast SC.SCUInt{}) -> False
      --  /mnt/applications/synopsys/2024-25/RHELx86/VC-STATIC_2024.09-SP1/hector/local/systemc/datatypes/bit/hector_lv_base.h:185:39: error: no member named 'is_01' in 'HectorLV<9>'
      --    bool is_01() const { return m_val.is_01(); }
      (SC.SCLV{}, ApplyMethod SC.Is01) -> False
      -- Error SYMAPI-001: Error: to_double functionality is currently not supported in sc_int.
      (SC.SCInt{}, ApplyMethod SC.ToDouble) -> False
      -- Error SYMAPI-001: Requested conversion from 'sc_dt::sc_uint' to non-scalar type 'sc_dt::sc_bigint' is not allowed.
      (SC.SCUInt{}, FunctionalCast SC.SCBigInt{}) -> False
      (SC.SCUInt{}, FunctionalCast SC.SCBigUInt{}) -> False
      --
      (_, Arithmetic _ _) -> False
      (_, _) -> True

oldVcfMods :: GenMods
oldVcfMods = GenMods{name = "oldVcf", transformationAllowed}
 where
  transformationAllowed e t =
    case (e.annotation, t) of
      (SC.SCInt{}, FunctionalCast SC.SCBigInt{}) -> False
      (SC.SCInt{}, FunctionalCast SC.SCBigUInt{}) -> False
      (SC.SCUInt{}, FunctionalCast SC.SCBigInt{}) -> False
      (SC.SCUInt{}, FunctionalCast SC.SCBigUInt{}) -> False
      (SC.CDouble, FunctionalCast SC.CInt) -> False
      (SC.CDouble, FunctionalCast SC.CUInt) -> False
      (SC.CDouble, CastWithAssignment SC.CInt) -> False
      (SC.CDouble, CastWithAssignment SC.CUInt) -> False
      (_, Arithmetic _ _) -> False
      (_, _) -> True

jasperMods :: GenMods
jasperMods = GenMods{name = "jasper", transformationAllowed}
 where
  transformationAllowed e t =
    case (e.annotation, t) of
      -- These types are not supported
      (_, FunctionalCast SC.SCBV{}) -> False
      (_, CastWithAssignment SC.SCBV{}) -> False
      (_, FunctionalCast SC.SCLV{}) -> False
      (_, CastWithAssignment SC.SCLV{}) -> False
      (_, FunctionalCast SC.SCLogic{}) -> False
      (_, CastWithAssignment SC.SCLogic{}) -> False
      (_, FunctionalCast SC.SCFixed{}) -> False
      (_, CastWithAssignment SC.SCFixed{}) -> False
      (_, FunctionalCast SC.SCUFixed{}) -> False
      (_, CastWithAssignment SC.SCUFixed{}) -> False
      -- Missing methods
      (SC.SCBigInt{}, ApplyMethod SC.ReduceXor) -> False
      (SC.SCBigInt{}, ApplyMethod SC.ReduceXNor) -> False
      (SC.SCBigUInt{}, ApplyMethod SC.ReduceXor) -> False
      (SC.SCBigUInt{}, ApplyMethod SC.ReduceXNor) -> False
      (SC.SCIntSubref{}, ApplyMethod _) -> False
      (SC.SCUIntSubref{}, ApplyMethod _) -> False
      (SC.SCSignedSubref{}, ApplyMethod _) -> False
      (SC.SCUnsignedSubref{}, ApplyMethod _) -> False
      (SC.SCIntSubref{}, CastWithAssignment SC.SCUInt{}) -> False
      (SC.SCIntSubref{}, CastWithAssignment SC.SCInt{}) -> False
      (SC.SCUIntSubref{}, CastWithAssignment SC.SCUInt{}) -> False
      (SC.SCUIntSubref{}, CastWithAssignment SC.SCInt{}) -> False
      (SC.SCSignedBitref{}, ApplyMethod SC.ToBool{}) -> False
      (SC.SCUnsignedBitref{}, ApplyMethod SC.ToBool{}) -> False
      --  "/scratch/mp5617/equifuzz_jasper_experiment/2a6b1053-fb00-4cb8-a1ad-5b076ba7e64b/spec.cpp", line 7: error: more than one operator "=" matches these operands:
      --      function "sc_dt::sc_int<SIZE>::operator=(uint64) [with SIZE=26]" (declared at line 67 of "/mnt/applications/cadence/2024-25/RHELx86/JASPER_2024.06.002/lib/c2rtl/sc_include/sysc/datatypes/int/sc_int.h")
      --      function "sc_dt::sc_int<SIZE>::operator=(const sc_dt::sc_int<SIZE> &) [with SIZE=26]" (declared at line 69 of "/mnt/applications/cadence/2024-25/RHELx86/JASPER_2024.06.002/lib/c2rtl/sc_include/sysc/datatypes/int/sc_int.h")
      (SC.SCIntBitref{}, CastWithAssignment SC.SCUInt{}) -> False
      (SC.SCUIntBitref{}, CastWithAssignment SC.SCUInt{}) -> False
      (SC.SCIntBitref{}, CastWithAssignment SC.SCInt{}) -> False
      (SC.SCUIntBitref{}, CastWithAssignment SC.SCInt{}) -> False
      -- Undetected undefined behaviour in large double-to-(u)int casts
      (SC.CDouble, CastWithAssignment SC.CInt) -> False
      (SC.CDouble, FunctionalCast SC.CInt) -> False
      (SC.CDouble, CastWithAssignment SC.SCInt{}) -> False
      (SC.CDouble, FunctionalCast SC.SCInt{}) -> False
      (SC.CDouble, CastWithAssignment SC.CUInt) -> False
      (SC.CDouble, FunctionalCast SC.CUInt) -> False
      (SC.CDouble, CastWithAssignment SC.SCUInt{}) -> False
      (SC.CDouble, FunctionalCast SC.SCUInt{}) -> False
      -- Trying to prevent undefined behaviour in programs like this:
      --     sc_dt::sc_uint<18>(sc_dt::sc_int<1>(-1)[0]);
      -- FIXME: We should detect when the experiment has undefined behaviour and
      -- be able to mark that in the UI
      (SC.SCInt w, Range{}) | w <= 1 -> False
      (SC.SCInt w, BitSelect{}) | w <= 1 -> False
      (SC.SCUInt w, Range{}) | w <= 1 -> False
      (SC.SCUInt w, BitSelect{}) | w <= 1 -> False
      (SC.SCBigInt w, Range{}) | w <= 1 -> False
      (SC.SCBigInt w, BitSelect{}) | w <= 1 -> False
      (SC.SCBigUInt w, Range{}) | w <= 1 -> False
      (SC.SCBigUInt w, BitSelect{}) | w <= 1 -> False
      -- Try to avoid bug triggered by this code:
      --   double dut() {
      --       int x0 = 1;
      --       return double(bool(x0));
      --   }
      (SC.CBool, FunctionalCast SC.CDouble) -> False
      (SC.CBool, CastWithAssignment SC.CDouble) -> False
      (_, _) -> True

slecMods :: GenMods
slecMods = GenMods{name = "slec", transformationAllowed}
 where
  transformationAllowed e t =
    case (e.annotation, t) of
      (_, FunctionalCast SC.SCFixed{}) -> False
      (_, CastWithAssignment SC.SCFixed{}) -> False
      (_, FunctionalCast SC.SCUFixed{}) -> False
      (_, CastWithAssignment SC.SCUFixed{}) -> False
      (SC.SCUInt{}, FunctionalCast SC.CDouble) -> False
      (SC.CDouble, FunctionalCast SC.SCUInt{}) -> False
      (SC.CDouble, FunctionalCast SC.SCInt{}) -> False
      (SC.CDouble, FunctionalCast SC.SCBigUInt{}) -> False
      (SC.CDouble, FunctionalCast SC.SCBigInt{}) -> False
      (SC.CDouble, CastWithAssignment SC.SCBigUInt{}) -> False
      (SC.CDouble, CastWithAssignment SC.SCBigInt{}) -> False
      (SC.CDouble, CastWithAssignment SC.SCInt{}) -> False
      (SC.CDouble, CastWithAssignment SC.SCUInt{}) -> False
      (_, _) -> True

noMods :: GenMods
noMods = GenMods{name = "none", transformationAllowed = \_ _ -> True}

modsMap :: Map String GenMods
modsMap =
  Map.fromList
    . map (\m -> (m.name, m))
    $ [ vcfMods
      , oldVcfMods
      , jasperMods
      , slecMods
      , noMods
      ]

-- FIXME: Orphan instances. Need to be here to break circular deps with
-- GenSystemC

instance Aeson.ToJSON GenMods where
  toJSON m = Aeson.object ["name" Aeson..= m.name]

instance Aeson.FromJSON GenMods where
  parseJSON = Aeson.withObject "GenMods" $ \v -> do
    name <- v .: "name"
    case Map.lookup name modsMap of
      Just m -> pure m
      Nothing -> fail ("Mods '" ++ name ++ "' not found")
