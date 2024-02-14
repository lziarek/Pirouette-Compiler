{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_constraints (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "constraints"
version :: Version
version = Version [0,14] []

synopsis :: String
synopsis = "Constraint manipulation"
copyright :: String
copyright = "Copyright (C) 2011-2021 Edward A. Kmett"
homepage :: String
homepage = "http://github.com/ekmett/constraints/"
