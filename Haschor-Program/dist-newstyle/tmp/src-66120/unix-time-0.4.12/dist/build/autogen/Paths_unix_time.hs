{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_unix_time (
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
version = Version [0,4,12] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/unix-time-0.4.12-e16abe3db2ba6202eaf67ba80810349b3d90e280f66a88f4c82f90dd309c9373/bin"
libdir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/unix-time-0.4.12-e16abe3db2ba6202eaf67ba80810349b3d90e280f66a88f4c82f90dd309c9373/lib"
dynlibdir  = "/home/ethanc9/.cabal/store/ghc-9.4.8/unix-time-0.4.12-e16abe3db2ba6202eaf67ba80810349b3d90e280f66a88f4c82f90dd309c9373/lib"
datadir    = "/home/ethanc9/.cabal/store/ghc-9.4.8/unix-time-0.4.12-e16abe3db2ba6202eaf67ba80810349b3d90e280f66a88f4c82f90dd309c9373/share"
libexecdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/unix-time-0.4.12-e16abe3db2ba6202eaf67ba80810349b3d90e280f66a88f4c82f90dd309c9373/libexec"
sysconfdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/unix-time-0.4.12-e16abe3db2ba6202eaf67ba80810349b3d90e280f66a88f4c82f90dd309c9373/etc"

getBinDir     = catchIO (getEnv "unix_time_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "unix_time_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "unix_time_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "unix_time_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unix_time_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "unix_time_sysconfdir") (\_ -> return sysconfdir)



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
