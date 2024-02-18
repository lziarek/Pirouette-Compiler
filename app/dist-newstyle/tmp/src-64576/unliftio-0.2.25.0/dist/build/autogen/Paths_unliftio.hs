{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_unliftio (
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
version = Version [0,2,25,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/unliftio-0.2.25.0-573006eb3bf555e9667a1b814a205702c2922d492c0d5bb4e15e41fe61530e07/bin"
libdir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/unliftio-0.2.25.0-573006eb3bf555e9667a1b814a205702c2922d492c0d5bb4e15e41fe61530e07/lib"
dynlibdir  = "/home/ethanc9/.cabal/store/ghc-9.4.8/unliftio-0.2.25.0-573006eb3bf555e9667a1b814a205702c2922d492c0d5bb4e15e41fe61530e07/lib"
datadir    = "/home/ethanc9/.cabal/store/ghc-9.4.8/unliftio-0.2.25.0-573006eb3bf555e9667a1b814a205702c2922d492c0d5bb4e15e41fe61530e07/share"
libexecdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/unliftio-0.2.25.0-573006eb3bf555e9667a1b814a205702c2922d492c0d5bb4e15e41fe61530e07/libexec"
sysconfdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/unliftio-0.2.25.0-573006eb3bf555e9667a1b814a205702c2922d492c0d5bb4e15e41fe61530e07/etc"

getBinDir     = catchIO (getEnv "unliftio_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "unliftio_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "unliftio_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "unliftio_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unliftio_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "unliftio_sysconfdir") (\_ -> return sysconfdir)



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
