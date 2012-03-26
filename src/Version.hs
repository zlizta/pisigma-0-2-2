module Version (version, Data.Version.showVersion) where

import Data.Version (Version(..),showVersion)

version :: Version
version = Version {versionBranch = [0,2,2], versionTags = []}