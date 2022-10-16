{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_bimaru_client (
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

bindir     = "C:\\Users\\Arno\\Desktop\\explorer\\Uni\\2022 ruduo\\PSI I\\FP2022\\.stack-work\\install\\384f5833\\bin"
libdir     = "C:\\Users\\Arno\\Desktop\\explorer\\Uni\\2022 ruduo\\PSI I\\FP2022\\.stack-work\\install\\384f5833\\lib\\x86_64-windows-ghc-8.10.7\\bimaru-client-0.1.0.0-H1zeS1OTVRZBRbns9T6wZJ"
dynlibdir  = "C:\\Users\\Arno\\Desktop\\explorer\\Uni\\2022 ruduo\\PSI I\\FP2022\\.stack-work\\install\\384f5833\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Users\\Arno\\Desktop\\explorer\\Uni\\2022 ruduo\\PSI I\\FP2022\\.stack-work\\install\\384f5833\\share\\x86_64-windows-ghc-8.10.7\\bimaru-client-0.1.0.0"
libexecdir = "C:\\Users\\Arno\\Desktop\\explorer\\Uni\\2022 ruduo\\PSI I\\FP2022\\.stack-work\\install\\384f5833\\libexec\\x86_64-windows-ghc-8.10.7\\bimaru-client-0.1.0.0"
sysconfdir = "C:\\Users\\Arno\\Desktop\\explorer\\Uni\\2022 ruduo\\PSI I\\FP2022\\.stack-work\\install\\384f5833\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bimaru_client_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bimaru_client_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bimaru_client_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bimaru_client_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bimaru_client_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bimaru_client_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
