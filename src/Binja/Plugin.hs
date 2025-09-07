{-# LANGUAGE ForeignFunctionInterface #-}

module Binja.Plugin
  ( initPlugins,
    getInstallDirectory,
    getBundledPluginDirectory,
    getUserDirectory,
    getRepositoriesDirectory,
    getUserPluginDirectory,
  )
where

import Binja.Types
import Binja.Utils
import Data.Bool (bool)

foreign import ccall "BNInitPlugins"
  c_BNInitPlugins :: CBool -> IO CBool

initPlugins :: Bool -> IO Bool
initPlugins = fmap toBool . c_BNInitPlugins . bool (CBool 0) (CBool 1)

foreign import ccall "BNGetInstallDirectory"
  c_BNGetInstallDirectory :: IO CString

getInstallDirectory :: IO String
getInstallDirectory = peekCString =<< c_BNGetInstallDirectory

foreign import ccall "BNGetBundledPluginDirectory"
  c_BNGetBundledPluginDirectory :: IO CString

getBundledPluginDirectory :: IO String
getBundledPluginDirectory = peekCString =<< c_BNGetBundledPluginDirectory

foreign import ccall "BNGetUserDirectory"
  c_BNGetUserDirectory :: IO CString

getUserDirectory :: IO String
getUserDirectory = peekCString =<< c_BNGetUserDirectory

foreign import ccall "BNGetUserPluginDirectory"
  c_BNGetUserPluginDirectory :: IO CString

getUserPluginDirectory :: IO String
getUserPluginDirectory = peekCString =<< c_BNGetUserPluginDirectory

foreign import ccall "BNGetRepositoriesDirectory"
  c_BNGetRepositoriesDirectory :: IO CString

getRepositoriesDirectory :: IO String
getRepositoriesDirectory = peekCString =<< c_BNGetRepositoriesDirectory
