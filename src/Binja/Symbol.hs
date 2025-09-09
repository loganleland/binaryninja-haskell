{-# LANGUAGE DuplicateRecordFields #-}

module Binja.Symbol
  ( Binja.Symbol.create,
    Binja.Symbol.codeRefs,
    Binja.Symbol.print,
  )
where

import Binja.FFI
import Binja.ReferenceSource
import Binja.Types
import Binja.Utils

ty :: BNSymbolPtr -> IO SymbolType
ty sym = do
  symTy <- c_BNGetSymbolType sym
  return $ toEnum $ fromIntegral symTy

binding :: BNSymbolPtr -> IO SymbolBinding
binding sym = do
  symBinding <- c_BNGetSymbolBinding sym
  return $ toEnum $ fromIntegral symBinding

name :: BNSymbolPtr -> IO String
name sym = do
  rawName <- c_BNGetSymbolRawName sym
  peekCString rawName

shortName :: BNSymbolPtr -> IO String
shortName sym = do
  short <- c_BNGetSymbolShortName sym
  peekCString short

fullName :: BNSymbolPtr -> IO String
fullName sym = do
  full <- c_BNGetSymbolFullName sym
  peekCString full

address :: BNSymbolPtr -> IO Word64
address sym = do
  c_BNGetSymbolAddress sym

ordinal :: BNSymbolPtr -> IO CInt
ordinal sym = do
  c_BNGetSymbolOrdinal sym

auto :: BNSymbolPtr -> IO Bool
auto sym = do
  toBool <$> c_BNIsSymbolAutoDefined sym

codeRefs :: BNBinaryViewPtr -> BNSymbolPtr -> IO [BNReferenceSource]
codeRefs view sym = do
  addr <- Binja.Symbol.address sym
  Binja.ReferenceSource.codeRefs view addr

create :: BNSymbolPtr -> IO Symbol
create sym = do
  nameStr <- Binja.Symbol.name sym
  t <- Binja.Symbol.ty sym
  b <- Binja.Symbol.binding sym
  addr <- Binja.Symbol.address sym
  a <- Binja.Symbol.auto sym
  return
    Binja.Types.Symbol
      { name = nameStr,
        ty = t,
        binding = b,
        address = addr,
        auto = a
      }

print :: BNSymbolPtr -> IO ()
print sym = do
  nameStr <- Binja.Symbol.name sym
  t <- Binja.Symbol.ty sym
  b <- Binja.Symbol.binding sym
  sname <- Binja.Symbol.shortName sym
  fname <- Binja.Symbol.fullName sym
  addr <- Binja.Symbol.address sym
  ord <- Binja.Symbol.ordinal sym
  isAuto <- Binja.Symbol.auto sym
  putStrLn "==============================="
  putStrLn ("Name      : " ++ nameStr)
  putStrLn ("Type      : " ++ show t)
  putStrLn ("Binding   : " ++ show b)
  putStrLn ("ShortName : " ++ sname)
  putStrLn ("FullName  : " ++ fname)
  putStrLn ("Address   : " ++ show addr)
  putStrLn ("Ordinal   : " ++ show ord)
  putStrLn ("Auto      : " ++ show isAuto)
