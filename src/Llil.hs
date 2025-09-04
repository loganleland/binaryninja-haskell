{-# LANGUAGE ForeignFunctionInterface #-}

module Llil
  ( Llil.startIndex
  , Llil.sourceFunc
  , Llil.fromRef
  , Llil.at
  ) where

import Types
import Function
import ReferenceSource
import BinaryView (functionsContaining)
import Utils (ptrToMaybe)


foreign import ccall unsafe "BNGetLowLevelILOwnerFunction"
  c_BNGetLowLevelILOwnderFunction :: BNLlilFunctionPtr -> IO BNFunctionPtr

sourceFunc :: BNLlilFunctionPtr -> IO (Maybe BNFunctionPtr)
sourceFunc func = do
  ptrToMaybe <$> c_BNGetLowLevelILOwnderFunction func


foreign import ccall unsafe "BNLowLevelILGetInstructionStart"
  c_BNLowLevelILGetInstructionStart
    :: BNLlilFunctionPtr -> BNArchPtr -> Word64 -> IO CSize


foreign import ccall unsafe "BNGetLowLevelILInstructionCount"
  c_BNGetLowLevelILInstructionCount :: BNLlilFunctionPtr -> IO CSize


startIndex :: BNLlilFunctionPtr -> BNArchPtr -> Word64 -> IO (Maybe CSize)
startIndex func arch addr = do
  if arch == nullPtr || func == nullPtr 
  then return Nothing
  else do
    startI <- c_BNLowLevelILGetInstructionStart func arch addr        
    count <- c_BNGetLowLevelILInstructionCount func
    -- Ensure start index is less than total llil instructions
    -- in function
    if startI >= count
    then return Nothing
    else return $ Just startI


foreign import ccall unsafe "BNGetLowLevelILIndexForInstruction"
  c_BNGetLowLevelILIndexForInstruction
    :: BNLlilFunctionPtr -> Word64 -> IO CSize


-- Convert an instruction index into an expression index
instIndexToExprIndex :: BNLlilFunctionPtr -> Word64 -> IO CSize
instIndexToExprIndex = c_BNGetLowLevelILIndexForInstruction


foreign import ccall unsafe "BNGetLowLevelILByIndexPtr"
  c_BNGetLowLevelILByIndexPtr
    :: Ptr BNLowLevelILInstruction -> BNLlilFunctionPtr -> CSize -> IO (Ptr BNLowLevelILInstruction)


llilByIndex :: BNLlilFunctionPtr -> CSize -> IO BNLowLevelILInstruction
llilByIndex func index =
  alloca $ \p -> do
  _ <- c_BNGetLowLevelILByIndexPtr p func index
  peek p


-- Retrieve the best LLIL instruction for the address in BNReferenceSource
fromRef :: BNReferenceSource -> IO (Maybe BNLowLevelILInstruction)
fromRef ref = do
  func <- Function.llil (bnFunc ref)
  sIndex <- startIndex func (bnArch ref) (bnAddr ref)
  case sIndex of
     Nothing -> return Nothing
     Just sIndex' -> do
       exprIndex <- instIndexToExprIndex func (fromIntegral sIndex')
       Just <$> llilByIndex func exprIndex


at :: BNBinaryViewPtr -> Word64 -> IO (Maybe BNLowLevelILInstruction)
at view addr = do
  rawFunc <- head <$> functionsContaining view addr
  llilFunc <- Function.llil rawFunc 
  sIndex <- startIndex llilFunc (Function.architecture rawFunc) addr
  case sIndex of
    Nothing -> return Nothing
    Just sIndex' -> do
      exprIndex <- instIndexToExprIndex llilFunc (fromIntegral sIndex')
      Just <$> llilByIndex llilFunc exprIndex

