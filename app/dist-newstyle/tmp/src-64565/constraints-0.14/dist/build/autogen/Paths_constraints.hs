{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_constraints (
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
version = Version [0,14] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/constraints-0.14-c9009aa9c1b8d9843467cc1401c1a9a89f522c7ec55ca21e184ec1f512fd34cd/bin"
libdir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/constraints-0.14-c9009aa9c1b8d9843467cc1401c1a9a89f522c7ec55ca21e184ec1f512fd34cd/lib"
dynlibdir  = "/home/ethanc9/.cabal/store/ghc-9.4.8/constraints-0.14-c9009aa9c1b8d9843467cc1401c1a9a89f522c7ec55ca21e184ec1f512fd34cd/lib"
datadir    = "/home/ethanc9/.cabal/store/ghc-9.4.8/constraints-0.14-c9009aa9c1b8d9843467cc1401c1a9a89f522c7ec55ca21e184ec1f512fd34cd/share"
libexecdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/constraints-0.14-c9009aa9c1b8d9843467cc1401c1a9a89f522c7ec55ca21e184ec1f512fd34cd/libexec"
sysconfdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/constraints-0.14-c9009aa9c1b8d9843467cc1401c1a9a89f522c7ec55ca21e184ec1f512fd34cd/etc"

getBinDir     = catchIO (getEnv "constraints_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "constraints_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "constraints_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "constraints_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "constraints_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "constraints_sysconfdir") (\_ -> return sysconfdir)



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
