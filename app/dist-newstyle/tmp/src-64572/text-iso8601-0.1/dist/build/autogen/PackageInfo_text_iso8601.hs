{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_text_iso8601 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "text_iso8601"
version :: Version
version = Version [0,1] []

synopsis :: String
synopsis = "Converting time to and from ISO 8601 text."
copyright :: String
copyright = "Oleg Grenrus <oleg.grenrus@iki.fi>"
homepage :: String
homepage = "https://github.com/haskell/aeson"
