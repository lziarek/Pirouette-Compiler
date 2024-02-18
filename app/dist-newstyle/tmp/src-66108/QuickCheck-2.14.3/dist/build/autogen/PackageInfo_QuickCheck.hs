{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_QuickCheck (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "QuickCheck"
version :: Version
version = Version [2,14,3] []

synopsis :: String
synopsis = "Automatic testing of Haskell programs"
copyright :: String
copyright = "2000-2019 Koen Claessen, 2006-2008 Bj\246rn Bringert, 2009-2019 Nick Smallbone"
homepage :: String
homepage = "https://github.com/nick8325/quickcheck"
