{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_crypton (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "crypton"
version :: Version
version = Version [0,34] []

synopsis :: String
synopsis = "Cryptography Primitives sink"
copyright :: String
copyright = "Vincent Hanquez <vincent@snarc.org>"
homepage :: String
homepage = "https://github.com/kazu-yamamoto/crypton"
