{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_vector (
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
version = Version [0,13,1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/vector-0.13.1.0-bf7e1cc6d1863625e3557a1ec930340ef7ddc87a95fa52ed2d17cafcd10fb626/bin"
libdir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/vector-0.13.1.0-bf7e1cc6d1863625e3557a1ec930340ef7ddc87a95fa52ed2d17cafcd10fb626/lib"
dynlibdir  = "/home/ethanc9/.cabal/store/ghc-9.4.8/vector-0.13.1.0-bf7e1cc6d1863625e3557a1ec930340ef7ddc87a95fa52ed2d17cafcd10fb626/lib"
datadir    = "/home/ethanc9/.cabal/store/ghc-9.4.8/vector-0.13.1.0-bf7e1cc6d1863625e3557a1ec930340ef7ddc87a95fa52ed2d17cafcd10fb626/share"
libexecdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/vector-0.13.1.0-bf7e1cc6d1863625e3557a1ec930340ef7ddc87a95fa52ed2d17cafcd10fb626/libexec"
sysconfdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/vector-0.13.1.0-bf7e1cc6d1863625e3557a1ec930340ef7ddc87a95fa52ed2d17cafcd10fb626/etc"

getBinDir     = catchIO (getEnv "vector_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "vector_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "vector_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "vector_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vector_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vector_sysconfdir") (\_ -> return sysconfdir)



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
