{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_crypton (
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
version = Version [0,34] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/crypton-0.34-0e9415cbac5391317a7f8ed91a78da9c03e59aa25272bd7c30ec3e17ca3a0f57/bin"
libdir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/crypton-0.34-0e9415cbac5391317a7f8ed91a78da9c03e59aa25272bd7c30ec3e17ca3a0f57/lib"
dynlibdir  = "/home/ethanc9/.cabal/store/ghc-9.4.8/crypton-0.34-0e9415cbac5391317a7f8ed91a78da9c03e59aa25272bd7c30ec3e17ca3a0f57/lib"
datadir    = "/home/ethanc9/.cabal/store/ghc-9.4.8/crypton-0.34-0e9415cbac5391317a7f8ed91a78da9c03e59aa25272bd7c30ec3e17ca3a0f57/share"
libexecdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/crypton-0.34-0e9415cbac5391317a7f8ed91a78da9c03e59aa25272bd7c30ec3e17ca3a0f57/libexec"
sysconfdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/crypton-0.34-0e9415cbac5391317a7f8ed91a78da9c03e59aa25272bd7c30ec3e17ca3a0f57/etc"

getBinDir     = catchIO (getEnv "crypton_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "crypton_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "crypton_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "crypton_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "crypton_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "crypton_sysconfdir") (\_ -> return sysconfdir)



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
