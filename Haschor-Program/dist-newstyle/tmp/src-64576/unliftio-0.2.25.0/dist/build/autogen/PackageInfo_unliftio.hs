{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_unliftio (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "unliftio"
version :: Version
version = Version [0,2,25,0] []

synopsis :: String
synopsis = "The MonadUnliftIO typeclass for unlifting monads to IO (batteries included)"
copyright :: String
copyright = "2017 FP Complete"
homepage :: String
homepage = "https://github.com/fpco/unliftio/tree/master/unliftio#readme"
