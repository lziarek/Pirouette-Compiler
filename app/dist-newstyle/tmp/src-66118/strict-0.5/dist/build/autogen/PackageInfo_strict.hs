{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_strict (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "strict"
version :: Version
version = Version [0,5] []

synopsis :: String
synopsis = "Strict data types and String IO."
copyright :: String
copyright = "(c) 2006-2008 by Roman Leshchinskiy\n(c) 2013-2014 by Simon Meier"
homepage :: String
homepage = "https://github.com/haskell-strict/strict"
