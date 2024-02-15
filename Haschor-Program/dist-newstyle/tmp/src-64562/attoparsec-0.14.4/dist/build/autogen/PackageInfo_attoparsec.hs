{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_attoparsec (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "attoparsec"
version :: Version
version = Version [0,14,4] []

synopsis :: String
synopsis = "Fast combinator parsing for bytestrings and text"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/haskell/attoparsec"
