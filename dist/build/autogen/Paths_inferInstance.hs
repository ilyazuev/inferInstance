{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_inferInstance (
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

bindir     = "/home/vagrant/.cabal/bin"
libdir     = "/home/vagrant/.cabal/lib/x86_64-linux-ghc-8.0.2/inferInstance-0.1.0.0-4OAtfHgWbQuIQFfU4K6eA9"
dynlibdir  = "/home/vagrant/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/vagrant/.cabal/share/x86_64-linux-ghc-8.0.2/inferInstance-0.1.0.0"
libexecdir = "/home/vagrant/.cabal/libexec"
sysconfdir = "/home/vagrant/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "inferInstance_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "inferInstance_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "inferInstance_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "inferInstance_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "inferInstance_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "inferInstance_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
