{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_JournalSystem (
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

bindir     = "/Users/magnusstrandberg/.cabal/bin"
libdir     = "/Users/magnusstrandberg/.cabal/lib/x86_64-osx-ghc-8.10.3/JournalSystem-0.1.0.0-inplace-JournalSystem"
dynlibdir  = "/Users/magnusstrandberg/.cabal/lib/x86_64-osx-ghc-8.10.3"
datadir    = "/Users/magnusstrandberg/.cabal/share/x86_64-osx-ghc-8.10.3/JournalSystem-0.1.0.0"
libexecdir = "/Users/magnusstrandberg/.cabal/libexec/x86_64-osx-ghc-8.10.3/JournalSystem-0.1.0.0"
sysconfdir = "/Users/magnusstrandberg/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "JournalSystem_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "JournalSystem_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "JournalSystem_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "JournalSystem_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "JournalSystem_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "JournalSystem_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
