{-# LANGUAGE ForeignFunctionInterface #-}
module Symbol
  ( ty
  , binding
  , name
  , shortName
  , fullName
  , Symbol.address
  , ordinal
  , auto
  , Symbol.codeRefs
  , Symbol.print
  ) where

import Types
import Utils
import qualified ReferenceSource as RS

foreign import ccall "BNGetSymbolType"
  c_BNGetSymbolType :: BNSymbolPtr -> IO CInt

ty :: BNSymbolPtr -> IO SymbolType
ty sym = do
  symTy <- c_BNGetSymbolType sym
  return $ toEnum $ fromIntegral symTy

foreign import ccall "BNGetSymbolBinding"
  c_BNGetSymbolBinding :: BNSymbolPtr -> IO CInt

binding :: BNSymbolPtr -> IO SymbolBinding
binding sym = do
  symBinding <- c_BNGetSymbolBinding sym
  return $ toEnum $ fromIntegral symBinding

foreign import ccall "BNGetSymbolRawName"
  c_BNGetSymbolRawName :: BNSymbolPtr -> IO CString

name :: BNSymbolPtr -> IO String
name sym = do
  rawName <- c_BNGetSymbolRawName sym
  peekCString rawName

foreign import ccall "BNGetSymbolShortName"
  c_BNGetSymbolShortName :: BNSymbolPtr -> IO CString

shortName :: BNSymbolPtr -> IO String
shortName sym = do
  short <- c_BNGetSymbolShortName sym
  peekCString short

foreign import ccall "BNGetSymbolFullName"
  c_BNGetSymbolFullName :: BNSymbolPtr -> IO CString

fullName :: BNSymbolPtr -> IO String
fullName sym = do
  full <- c_BNGetSymbolFullName sym
  peekCString full

foreign import ccall "BNGetSymbolAddress"
  c_BNGetSymbolAddress :: BNSymbolPtr -> IO Word64

address :: BNSymbolPtr -> IO Word64
address sym = do
  c_BNGetSymbolAddress sym

foreign import ccall "BNGetSymbolOrdinal"
  c_BNGetSymbolOrdinal :: BNSymbolPtr -> IO CInt

ordinal :: BNSymbolPtr -> IO CInt
ordinal sym = do
  c_BNGetSymbolOrdinal sym

foreign import ccall "BNIsSymbolAutoDefined"
  c_BNIsSymbolAutoDefined :: BNSymbolPtr -> IO CBool

auto :: BNSymbolPtr -> IO Bool
auto sym = do
  toBool <$> c_BNIsSymbolAutoDefined sym

codeRefs :: BNBinaryViewPtr -> BNSymbolPtr -> IO [RS.BNReferenceSource]
codeRefs view sym = do
  addr <- Symbol.address sym
  RS.codeRefs view addr
  

print :: BNSymbolPtr -> IO ()
print sym = do
  nameStr <- name sym
  t       <- ty sym
  b       <- binding sym
  sname   <- shortName sym
  fname   <- fullName sym
  addr    <- Symbol.address sym
  ord     <- ordinal sym
  isAuto  <- auto sym
  putStrLn "==============================="
  putStrLn ("Name      : " ++ nameStr)
  putStrLn ("Type      : " ++ show t)
  putStrLn ("Binding   : " ++ show b)
  putStrLn ("ShortName : " ++ sname)
  putStrLn ("FullName  : " ++ fname)
  putStrLn ("Address   : " ++ show addr)
  putStrLn ("Ordinal   : " ++ show ord)
  putStrLn ("Auto      : " ++ show isAuto)


