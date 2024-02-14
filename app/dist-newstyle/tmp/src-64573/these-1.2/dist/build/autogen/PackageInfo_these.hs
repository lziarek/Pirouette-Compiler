{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_these (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "these"
version :: Version
version = Version [1,2] []

synopsis :: String
synopsis = "An either-or-both data type."
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/haskellari/these"
