{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_fuck (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/blackcap/.cabal/bin"
libdir     = "/home/blackcap/.cabal/lib/x86_64-linux-ghc-8.0.2/fuck-0.1.0.0-EwiY2Yl7ELs8DLBVT8uRh2"
dynlibdir  = "/home/blackcap/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/blackcap/.cabal/share/x86_64-linux-ghc-8.0.2/fuck-0.1.0.0"
libexecdir = "/home/blackcap/.cabal/libexec"
sysconfdir = "/home/blackcap/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fuck_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fuck_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fuck_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fuck_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fuck_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fuck_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
