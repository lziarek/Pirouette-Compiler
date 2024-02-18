{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_comonad (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "comonad"
version :: Version
version = Version [5,0,8] []

synopsis :: String
synopsis = "Comonads"
copyright :: String
copyright = "Copyright (C) 2008-2014 Edward A. Kmett,\nCopyright (C) 2004-2008 Dave Menendez"
homepage :: String
homepage = "http://github.com/ekmett/comonad/"
