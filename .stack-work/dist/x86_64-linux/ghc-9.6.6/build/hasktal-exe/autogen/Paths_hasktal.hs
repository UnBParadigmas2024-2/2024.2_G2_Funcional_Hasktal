{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hasktal (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/marcos/faculdade/Paradigmas/2024.2_G2_Funcional_Hasktal/.stack-work/install/x86_64-linux/e1eaa6925ee4e39dab3dddeff1c6ddb7d2be263573fbabc87015a64a812139ff/9.6.6/bin"
libdir     = "/home/marcos/faculdade/Paradigmas/2024.2_G2_Funcional_Hasktal/.stack-work/install/x86_64-linux/e1eaa6925ee4e39dab3dddeff1c6ddb7d2be263573fbabc87015a64a812139ff/9.6.6/lib/x86_64-linux-ghc-9.6.6/hasktal-0.1.0.0-FHzJBRqNp6YFwlXw23Hb1l-hasktal-exe"
dynlibdir  = "/home/marcos/faculdade/Paradigmas/2024.2_G2_Funcional_Hasktal/.stack-work/install/x86_64-linux/e1eaa6925ee4e39dab3dddeff1c6ddb7d2be263573fbabc87015a64a812139ff/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/marcos/faculdade/Paradigmas/2024.2_G2_Funcional_Hasktal/.stack-work/install/x86_64-linux/e1eaa6925ee4e39dab3dddeff1c6ddb7d2be263573fbabc87015a64a812139ff/9.6.6/share/x86_64-linux-ghc-9.6.6/hasktal-0.1.0.0"
libexecdir = "/home/marcos/faculdade/Paradigmas/2024.2_G2_Funcional_Hasktal/.stack-work/install/x86_64-linux/e1eaa6925ee4e39dab3dddeff1c6ddb7d2be263573fbabc87015a64a812139ff/9.6.6/libexec/x86_64-linux-ghc-9.6.6/hasktal-0.1.0.0"
sysconfdir = "/home/marcos/faculdade/Paradigmas/2024.2_G2_Funcional_Hasktal/.stack-work/install/x86_64-linux/e1eaa6925ee4e39dab3dddeff1c6ddb7d2be263573fbabc87015a64a812139ff/9.6.6/etc"

getBinDir     = catchIO (getEnv "hasktal_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hasktal_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hasktal_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hasktal_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hasktal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hasktal_sysconfdir") (\_ -> return sysconfdir)



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
