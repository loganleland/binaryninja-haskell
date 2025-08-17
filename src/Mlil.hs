module Mlil
  ( Mlil.fromRef
  ) where

import Types
import ReferenceSource
import Function


foreign import ccall unsafe "BNMediumLevelILGetInstructionStart"
  c_BNMediumLevelILGetInstructionStart
    :: BNMlilFunctionPtr -> BNArchPtr -> Word64 -> IO CSize


foreign import ccall unsafe "BNGetMediumLevelILInstructionCount"
  c_BNGetMediumLevelILInstructionCount :: BNMlilFunctionPtr -> IO CSize


foreign import ccall unsafe "BNGetMediumLevelILIndexForInstruction"
  c_BNGetMediumLevelILIndexForInstruction
    :: BNMlilFunctionPtr -> Word64 -> IO CSize

foreign import ccall unsafe "BNGetMediumLevelILByIndexPtr"
  c_BNGetMediumLevelILByIndexPtr
    :: BNMlilFunctionPtr -> CSize -> IO (Ptr BNMediumLevelILInstruction)

startIndex :: BNMlilFunctionPtr -> BNArchPtr -> Word64 -> IO (Maybe CSize)
startIndex func arch addr = do
  if arch == nullPtr || func == nullPtr 
  then return Nothing
  else do
    start <- c_BNMediumLevelILGetInstructionStart func arch addr        
    count <- c_BNGetMediumLevelILInstructionCount func
    -- Ensure start index is less than total mlil instructions
    -- in function
    if start >= count
    then return Nothing
    else return $ Just start



-- Convert an instruction index into an expression index
instIndexToExprIndex :: BNMlilFunctionPtr -> Word64 -> IO CSize
instIndexToExprIndex = c_BNGetMediumLevelILIndexForInstruction


mlilByIndex :: BNMlilFunctionPtr -> CSize -> IO (Maybe BNMediumLevelILInstruction)
mlilByIndex func index = do
  p <- c_BNGetMediumLevelILByIndexPtr func index
  if p == nullPtr
  then return Nothing
  else Just <$> peek p


-- Retrieve the best MLIL instruction for the address in BNReferenceSource
fromRef :: BNReferenceSource -> IO (Maybe BNMediumLevelILInstruction)
fromRef ref = do
  func <- mlil (bnFunc ref)
  case func of
    Nothing -> return Nothing
    Just func' -> do
      sIndex <- startIndex func' (bnArch ref) (bnAddr ref)
      case sIndex of
        Nothing -> return Nothing
        Just sIndex' -> do
          exprIndex <- instIndexToExprIndex func' (fromIntegral sIndex')
          mlilByIndex func' exprIndex

