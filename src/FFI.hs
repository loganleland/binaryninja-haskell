{-# LANGUAGE ForeignFunctionInterface #-}

module FFI where

import Foreign.C.String (CString, peekCString, withCString)

foreign import ccall "BNGetProduct"
  c_BNGetProduct :: IO CString

getProduct :: IO String
getProduct = do
  cStr <- c_BNGetProduct
  peekCString cStr

foreign import ccall "BNGetProductType"
  c_BNGetProductType :: IO CString

getProductType :: IO String
getProductType = do
  cStr <- c_BNGetProductType
  peekCString cStr

foreign import ccall "BNGetLicensedUserEmail"
  c_BNGetLicensedUserEmail :: IO CString

getLicensedUserEmail :: IO String
getLicensedUserEmail = do
  cStr <- c_BNGetLicensedUserEmail
  peekCString cStr

foreign import ccall "BNGetSerialNumber"
  c_BNGetSerialNumber :: IO CString

getSerialNumberString :: IO String
getSerialNumberString = do
  cStr <- c_BNGetSerialNumber
  peekCString cStr

foreign import ccall "BNGetVersionString"
  c_BNGetVersionString :: IO CString

getVersionString :: IO String
getVersionString = do
  cStr <- c_BNGetVersionString
  peekCString cStr

foreign import ccall "BNSetLicense"
  c_BNSetLicense :: CString -> IO ()

setLicense :: String -> IO ()
setLicense licenseData = withCString licenseData c_BNSetLicense

foreign import ccall "BNShutdown"
  c_BNShutdown :: IO ()

shutdown :: IO ()
shutdown = c_BNShutdown

foreign import ccall "BNGetUniqueIdentifierString"
  c_BNGetUniqueIdentifierString :: IO CString

getUniqueIdentifierString :: IO String
getUniqueIdentifierString = do
  cStr <- c_BNGetUniqueIdentifierString
  peekCString cStr

foreign import ccall "BNGetSettingsFileName"
  c_BNGetSettingsFileName :: IO CString

getSettingsFileName :: IO String
getSettingsFileName = do
  cStr <- c_BNGetSettingsFileName
  peekCString cStr
