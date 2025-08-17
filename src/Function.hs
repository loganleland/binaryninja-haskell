{-# LANGUAGE ForeignFunctionInterface #-}
module Function (
    Function
  , start
  , highestAddress
  , lowestAddress
  , symbol
  , auto
  , architecture 
  , hasUserAnnotations
  , hasExplicitlyDefinedType
  , needsUpdate
  , hasUnresolvedIndirectBranches
  , getComment
  , setComment
  , Function.llil
  , Function.mlil
  , Function.print
  ) where

import Utils
import Types
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


symbol :: BNFunctionPtr -> IO (Maybe BNSymbolPtr)
symbol = fmap ptrToMaybe . c_BNGetFunctionSymbol


foreign import ccall unsafe "BNWasFunctionAutomaticallyDiscovered"
  c_BNWasFunctionAutomaticallyDiscovered :: BNFunctionPtr -> IO CBool


auto :: BNFunctionPtr -> IO Bool
auto = fmap Utils.toBool . c_BNWasFunctionAutomaticallyDiscovered


foreign import ccall unsafe "BNFunctionHasUserAnnotations"
  c_BNFunctionHasUserAnnotations :: BNFunctionPtr -> IO CBool


hasUserAnnotations :: BNFunctionPtr -> IO Bool
hasUserAnnotations = fmap Utils.toBool . c_BNFunctionHasUserAnnotations


foreign import ccall unsafe "BNFunctionHasExplicitlyDefinedType"
  c_BNFunctionHasExplicitlyDefinedType :: BNFunctionPtr -> IO CBool


hasExplicitlyDefinedType :: BNFunctionPtr -> IO Bool
hasExplicitlyDefinedType = fmap Utils.toBool . c_BNFunctionHasExplicitlyDefinedType


foreign import ccall unsafe "BNIsFunctionUpdateNeeded"
  c_BNIsFunctionUpdateNeeded :: BNFunctionPtr -> IO CBool


needsUpdate :: BNFunctionPtr -> IO Bool
needsUpdate = fmap Utils.toBool . c_BNIsFunctionUpdateNeeded


foreign import ccall unsafe "BNHasUnresolvedIndirectBranches"
  c_BNHasUnresolvedIndirectBranches :: BNFunctionPtr -> IO CBool


hasUnresolvedIndirectBranches :: BNFunctionPtr -> IO Bool
hasUnresolvedIndirectBranches = fmap Utils.toBool . c_BNHasUnresolvedIndirectBranches


foreign import ccall "BNGetFunctionComment"
  c_BNGetFunctionComment :: BNFunctionPtr -> IO CString


getComment :: BNFunctionPtr -> IO String
getComment func =  do
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


llil :: BNFunctionPtr -> IO (Maybe BNLlilFunctionPtr)
llil func = do
  if func == nullPtr then return Nothing
  else do
    llilFuncPtr <- c_BNGetFunctionLowLevelIL func
    return $ ptrToMaybe llilFuncPtr 


foreign import ccall unsafe "BNGetFunctionMediumLevelIL"
  c_BNGetFunctionMediumLevelIL :: BNFunctionPtr -> IO BNMlilFunctionPtr


mlil :: BNFunctionPtr -> IO (Maybe BNMlilFunctionPtr)
mlil func = do
  if func == nullPtr then return Nothing
  else do
    mlilFuncPtr <- c_BNGetFunctionMediumLevelIL func
    return $ ptrToMaybe mlilFuncPtr 


print :: BNFunctionPtr -> IO ()
print func = do
  s        <- start func
  hi       <- highestAddress func
  lo       <- lowestAddress func
  mSym     <- symbol func
  isAuto   <- auto func
  userAnn  <- hasUserAnnotations func
  explTy   <- hasExplicitlyDefinedType func
  upd      <- needsUpdate func
  indBr    <- hasUnresolvedIndirectBranches func
  cmt      <- getComment func
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
    putStrLn $ "  comment:  " ++ cmt

