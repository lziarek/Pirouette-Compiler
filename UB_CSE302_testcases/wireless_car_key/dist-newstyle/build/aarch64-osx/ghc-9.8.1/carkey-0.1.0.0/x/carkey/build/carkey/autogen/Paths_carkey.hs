{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_carkey (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/miniv/.cabal/bin"
libdir     = "/Users/miniv/.cabal/lib/aarch64-osx-ghc-9.8.1/carkey-0.1.0.0-inplace-carkey"
dynlibdir  = "/Users/miniv/.cabal/lib/aarch64-osx-ghc-9.8.1"
datadir    = "/Users/miniv/.cabal/share/aarch64-osx-ghc-9.8.1/carkey-0.1.0.0"
libexecdir = "/Users/miniv/.cabal/libexec/aarch64-osx-ghc-9.8.1/carkey-0.1.0.0"
sysconfdir = "/Users/miniv/.cabal/etc"

getBinDir     = catchIO (getEnv "carkey_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "carkey_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "carkey_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "carkey_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "carkey_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "carkey_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
