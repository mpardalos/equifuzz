{-# LANGUAGE CPP #-}
module Meta where

import Data.Version
import Paths_equifuzz (version)

versionName :: String
versionName = showVersion version <>
#ifdef EVALUATION_VERSION
      " (evaluation)"
#else
      " (full)"
#endif
