{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_comonad (
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
version = Version [5,0,8] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/comonad-5.0.8-b36b464bd7b34d6cc1447197f93345806c4641060e5de7e542accc923a1dd8b7/bin"
libdir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/comonad-5.0.8-b36b464bd7b34d6cc1447197f93345806c4641060e5de7e542accc923a1dd8b7/lib"
dynlibdir  = "/home/ethanc9/.cabal/store/ghc-9.4.8/comonad-5.0.8-b36b464bd7b34d6cc1447197f93345806c4641060e5de7e542accc923a1dd8b7/lib"
datadir    = "/home/ethanc9/.cabal/store/ghc-9.4.8/comonad-5.0.8-b36b464bd7b34d6cc1447197f93345806c4641060e5de7e542accc923a1dd8b7/share"
libexecdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/comonad-5.0.8-b36b464bd7b34d6cc1447197f93345806c4641060e5de7e542accc923a1dd8b7/libexec"
sysconfdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/comonad-5.0.8-b36b464bd7b34d6cc1447197f93345806c4641060e5de7e542accc923a1dd8b7/etc"

getBinDir     = catchIO (getEnv "comonad_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "comonad_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "comonad_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "comonad_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "comonad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "comonad_sysconfdir") (\_ -> return sysconfdir)



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
