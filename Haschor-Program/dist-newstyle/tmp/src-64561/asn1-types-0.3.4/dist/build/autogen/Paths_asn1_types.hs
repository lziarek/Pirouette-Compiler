{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_asn1_types (
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
version = Version [0,3,4] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/asn1-types-0.3.4-7b7cb61c4414c3ef3522b61af80cd57a8800800381a77ac7395bac16137f281c/bin"
libdir     = "/home/ethanc9/.cabal/store/ghc-9.4.8/asn1-types-0.3.4-7b7cb61c4414c3ef3522b61af80cd57a8800800381a77ac7395bac16137f281c/lib"
dynlibdir  = "/home/ethanc9/.cabal/store/ghc-9.4.8/asn1-types-0.3.4-7b7cb61c4414c3ef3522b61af80cd57a8800800381a77ac7395bac16137f281c/lib"
datadir    = "/home/ethanc9/.cabal/store/ghc-9.4.8/asn1-types-0.3.4-7b7cb61c4414c3ef3522b61af80cd57a8800800381a77ac7395bac16137f281c/share"
libexecdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/asn1-types-0.3.4-7b7cb61c4414c3ef3522b61af80cd57a8800800381a77ac7395bac16137f281c/libexec"
sysconfdir = "/home/ethanc9/.cabal/store/ghc-9.4.8/asn1-types-0.3.4-7b7cb61c4414c3ef3522b61af80cd57a8800800381a77ac7395bac16137f281c/etc"

getBinDir     = catchIO (getEnv "asn1_types_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "asn1_types_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "asn1_types_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "asn1_types_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "asn1_types_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "asn1_types_sysconfdir") (\_ -> return sysconfdir)



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
