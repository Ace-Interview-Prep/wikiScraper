{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_cryptohash (
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
version = Version [0,11,9] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/lazylambda/.cabal/store/ghc-8.10.7/cryptohash-0.11.9-616354cdc3049246f79ea62e3c027f96d8a34665ea7772eadfbe4bba3e28bc19/bin"
libdir     = "/home/lazylambda/.cabal/store/ghc-8.10.7/cryptohash-0.11.9-616354cdc3049246f79ea62e3c027f96d8a34665ea7772eadfbe4bba3e28bc19/lib"
dynlibdir  = "/home/lazylambda/.cabal/store/ghc-8.10.7/cryptohash-0.11.9-616354cdc3049246f79ea62e3c027f96d8a34665ea7772eadfbe4bba3e28bc19/lib"
datadir    = "/home/lazylambda/.cabal/store/ghc-8.10.7/cryptohash-0.11.9-616354cdc3049246f79ea62e3c027f96d8a34665ea7772eadfbe4bba3e28bc19/share"
libexecdir = "/home/lazylambda/.cabal/store/ghc-8.10.7/cryptohash-0.11.9-616354cdc3049246f79ea62e3c027f96d8a34665ea7772eadfbe4bba3e28bc19/libexec"
sysconfdir = "/home/lazylambda/.cabal/store/ghc-8.10.7/cryptohash-0.11.9-616354cdc3049246f79ea62e3c027f96d8a34665ea7772eadfbe4bba3e28bc19/etc"

getBinDir     = catchIO (getEnv "cryptohash_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "cryptohash_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "cryptohash_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "cryptohash_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cryptohash_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cryptohash_sysconfdir") (\_ -> return sysconfdir)




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
