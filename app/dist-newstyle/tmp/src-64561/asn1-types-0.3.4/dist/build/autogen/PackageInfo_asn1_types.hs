{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_asn1_types (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "asn1_types"
version :: Version
version = Version [0,3,4] []

synopsis :: String
synopsis = "ASN.1 types"
copyright :: String
copyright = "Vincent Hanquez <vincent@snarc.org>"
homepage :: String
homepage = "http://github.com/vincenthz/hs-asn1"
