module ToolRestrictions where

import GenSystemC (TypeOperationsMod)

vcfMods :: TypeOperationsMod
vcfMods = noMods

noMods :: TypeOperationsMod
noMods _ = id
