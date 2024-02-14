{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_recv (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "recv"
version :: Version
version = Version [0,1,0] []

synopsis :: String
synopsis = "Efficient network recv"
copyright :: String
copyright = ""
homepage :: String
homepage = "http://github.com/yesodweb/wai"
