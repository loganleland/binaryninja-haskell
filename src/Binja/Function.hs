{-# LANGUAGE ForeignFunctionInterface #-}

module Binja.Function
  ( Function,
    Binja.Function.start,
    Binja.Function.highestAddress,
    Binja.Function.lowestAddress,
    Binja.Function.symbol,
    Binja.Function.auto,
    Binja.Function.architecture,
    Binja.Function.hasUserAnnotations,
    Binja.Function.hasExplicitlyDefinedType,
    Binja.Function.needsUpdate,
    Binja.Function.hasUnresolvedIndirectBranches,
    Binja.Function.getComment,
    Binja.Function.setComment,
    Binja.Function.llil,
    Binja.Function.mlil,
    Binja.Function.mlilToSSA,
    Binja.Function.mlilSSA,
    Binja.Function.mlilToRawFunction,
    Binja.Function.print,
  )
where

import Binja.Types
import Binja.Utils
import Binja.Symbol
import Control.Monad (unless)

foreign import ccall unsafe "BNGetFunctionStart"
  c_BNGetFunctionStart :: BNFunctionPtr -> IO Word64

start :: BNFunctionPtr -> IO Word64
start = c_BNGetFunctionStart

foreign import ccall unsafe "BNGetFunctionHighestAddress"
  c_BNGetFunctionHighestAddress :: BNFunctionPtr -> IO Word64

highestAddress :: BNFunctionPtr -> IO Word64
highestAddress = c_BNGetFunctionHighestAddress

foreign import ccall unsafe "BNGetFunctionLowestAddress"
  c_BNGetFunctionLowestAddress :: BNFunctionPtr -> IO Word64

lowestAddress :: BNFunctionPtr -> IO Word64
lowestAddress = c_BNGetFunctionLowestAddress

foreign import ccall unsafe "BNGetFunctionSymbol"
  c_BNGetFunctionSymbol :: BNFunctionPtr -> IO BNSymbolPtr

symbol :: BNFunctionPtr -> IO Symbol
symbol func = do
  p <- c_BNGetFunctionSymbol func
  if p == nullPtr
    then error "c_BNGetFunctionSymbol returned null"
    else Binja.Symbol.create p

foreign import ccall unsafe "BNWasFunctionAutomaticallyDiscovered"
  c_BNWasFunctionAutomaticallyDiscovered :: BNFunctionPtr -> IO CBool

auto :: BNFunctionPtr -> IO Bool
auto = fmap Binja.Utils.toBool . c_BNWasFunctionAutomaticallyDiscovered

foreign import ccall unsafe "BNFunctionHasUserAnnotations"
  c_BNFunctionHasUserAnnotations :: BNFunctionPtr -> IO CBool

hasUserAnnotations :: BNFunctionPtr -> IO Bool
hasUserAnnotations = fmap Binja.Utils.toBool . c_BNFunctionHasUserAnnotations

foreign import ccall unsafe "BNFunctionHasExplicitlyDefinedType"
  c_BNFunctionHasExplicitlyDefinedType :: BNFunctionPtr -> IO CBool

hasExplicitlyDefinedType :: BNFunctionPtr -> IO Bool
hasExplicitlyDefinedType = fmap Binja.Utils.toBool . c_BNFunctionHasExplicitlyDefinedType

foreign import ccall unsafe "BNIsFunctionUpdateNeeded"
  c_BNIsFunctionUpdateNeeded :: BNFunctionPtr -> IO CBool

needsUpdate :: BNFunctionPtr -> IO Bool
needsUpdate = fmap Binja.Utils.toBool . c_BNIsFunctionUpdateNeeded

foreign import ccall unsafe "BNHasUnresolvedIndirectBranches"
  c_BNHasUnresolvedIndirectBranches :: BNFunctionPtr -> IO CBool

hasUnresolvedIndirectBranches :: BNFunctionPtr -> IO Bool
hasUnresolvedIndirectBranches = fmap Binja.Utils.toBool . c_BNHasUnresolvedIndirectBranches

foreign import ccall "BNGetFunctionComment"
  c_BNGetFunctionComment :: BNFunctionPtr -> IO CString

getComment :: BNFunctionPtr -> IO String
getComment func = do
  cStr <- c_BNGetFunctionComment func
  peekCString cStr

foreign import ccall "BNSetFunctionComment"
  c_BNSetFunctionComment :: BNFunctionPtr -> CString -> IO ()

foreign import ccall "BNGetFunctionArchitecture"
  c_BNGetFunctionArchitecture :: BNFunctionPtr -> BNArchPtr

architecture :: BNFunctionPtr -> BNArchPtr
architecture = c_BNGetFunctionArchitecture

setComment :: BNFunctionPtr -> String -> IO ()
setComment func comment = do
  cStr <- newCString comment
  c_BNSetFunctionComment func cStr

foreign import ccall unsafe "BNGetFunctionLowLevelIL"
  c_BNGetFunctionLowLevelIL :: BNFunctionPtr -> IO BNLlilFunctionPtr

llil :: BNFunctionPtr -> IO BNLlilFunctionPtr
llil func = do
  if func == nullPtr
    then error "llil: func == nullPtr"
    else do
      llilFuncPtr <- c_BNGetFunctionLowLevelIL func
      if llilFuncPtr == nullPtr
        then error "llil: c_BNGetFunctionLowLevelIL returned nullPtr"
        else return llilFuncPtr

foreign import ccall unsafe "BNGetFunctionMediumLevelIL"
  c_BNGetFunctionMediumLevelIL :: BNFunctionPtr -> IO BNMlilFunctionPtr

foreign import ccall unsafe "BNGetMediumLevelILSSAForm"
  c_BNGetMediumLevelILSSAForm :: BNMlilFunctionPtr -> IO BNMlilSSAFunctionPtr

foreign import ccall unsafe "BNGetMediumLevelILOwnerFunction"
  c_BNGetMediumLevelILOwnerFunction :: BNMlilSSAFunctionPtr -> IO BNFunctionPtr

mlilToRawFunction :: BNMlilSSAFunctionPtr -> IO BNFunctionPtr
mlilToRawFunction func = do
  rawFunc <- c_BNGetMediumLevelILOwnerFunction func
  if rawFunc == nullPtr
    then error "mlilToRawFunction: BNGetMediumLevelILOwnerFunction returned null"
    else return rawFunc

mlil :: BNFunctionPtr -> IO BNMlilFunctionPtr
mlil func = do
  if func == nullPtr
    then error "mlil: func == nullPtr"
    else do
      mlilFuncPtr <- c_BNGetFunctionMediumLevelIL func
      if mlilFuncPtr == nullPtr
        then error "mlil: c_BNGetFunctionMediumLevelIL returned nullPtr"
        else return mlilFuncPtr

mlilToSSA :: BNMlilFunctionPtr -> IO BNMlilSSAFunctionPtr
mlilToSSA func = do
  p <- c_BNGetMediumLevelILSSAForm func
  if p == nullPtr
    then error "mlilToSSA: c_BNGetMediumLevelILSSAForm returned nullPtr"
    else return p

mlilSSA :: BNFunctionPtr -> IO BNMlilSSAFunctionPtr
mlilSSA func = do
  mlilFunc <- mlil func
  c_BNGetMediumLevelILSSAForm mlilFunc

print :: BNFunctionPtr -> IO ()
print func = do
  s <- start func
  hi <- highestAddress func
  lo <- lowestAddress func
  mSym <- symbol func
  isAuto <- Binja.Function.auto func
  userAnn <- hasUserAnnotations func
  explTy <- hasExplicitlyDefinedType func
  upd <- needsUpdate func
  indBr <- hasUnresolvedIndirectBranches func
  cmt <- getComment func
  putStrLn "== BNFunction =="
  putStrLn $ "  start: " ++ show s
  putStrLn $ "  lowestAddress: " ++ show lo
  putStrLn $ "  highestAddress: " ++ show hi
  putStrLn $ "  hasSymbol: " ++ show mSym
  putStrLn $ "  automaticallyDiscovered: " ++ show isAuto
  putStrLn $ "  hasUserAnnotations: " ++ show userAnn
  putStrLn $ "  hasExplicitlyDefinedType: " ++ show explTy
  putStrLn $ "  needsUpdate: " ++ show upd
  putStrLn $ "  unresolvedIndirectBranches: " ++ show indBr
  unless (null cmt) $
    putStrLn $
      "  comment:  " ++ cmt
