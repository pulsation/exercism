{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_difference_of_squares (
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
version = Version [1,2,0,5] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/phil/exercism/haskell/difference-of-squares/.stack-work/install/x86_64-linux-nix/lts-10.2/8.2.2/bin"
libdir     = "/home/phil/exercism/haskell/difference-of-squares/.stack-work/install/x86_64-linux-nix/lts-10.2/8.2.2/lib/x86_64-linux-ghc-8.2.2/difference-of-squares-1.2.0.5-GjQntn8vLZlFRy4cQTJxnH"
dynlibdir  = "/home/phil/exercism/haskell/difference-of-squares/.stack-work/install/x86_64-linux-nix/lts-10.2/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/phil/exercism/haskell/difference-of-squares/.stack-work/install/x86_64-linux-nix/lts-10.2/8.2.2/share/x86_64-linux-ghc-8.2.2/difference-of-squares-1.2.0.5"
libexecdir = "/home/phil/exercism/haskell/difference-of-squares/.stack-work/install/x86_64-linux-nix/lts-10.2/8.2.2/libexec/x86_64-linux-ghc-8.2.2/difference-of-squares-1.2.0.5"
sysconfdir = "/home/phil/exercism/haskell/difference-of-squares/.stack-work/install/x86_64-linux-nix/lts-10.2/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "difference_of_squares_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "difference_of_squares_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "difference_of_squares_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "difference_of_squares_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "difference_of_squares_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "difference_of_squares_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
