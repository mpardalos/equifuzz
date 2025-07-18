module Meta where

import Data.Version (showVersion)
import Paths_equifuzz (version)

versionName :: String
versionName = showVersion version
