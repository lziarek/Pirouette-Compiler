{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_unix_time (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "unix_time"
version :: Version
version = Version [0,4,12] []

synopsis :: String
synopsis = "Unix time parser/formatter and utilities"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
