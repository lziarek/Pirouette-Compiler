{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_prettyprinter_ansi_terminal (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "prettyprinter_ansi_terminal"
version :: Version
version = Version [1,1,3] []

synopsis :: String
synopsis = "ANSI terminal backend for the \187prettyprinter\171 package."
copyright :: String
copyright = ""
homepage :: String
homepage = "http://github.com/quchen/prettyprinter"
