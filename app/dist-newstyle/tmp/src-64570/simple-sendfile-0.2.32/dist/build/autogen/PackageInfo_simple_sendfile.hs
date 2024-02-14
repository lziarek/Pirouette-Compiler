{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_simple_sendfile (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "simple_sendfile"
version :: Version
version = Version [0,2,32] []

synopsis :: String
synopsis = "Cross platform library for the sendfile system call"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
