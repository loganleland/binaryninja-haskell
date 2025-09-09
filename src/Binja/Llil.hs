module Binja.Llil
  ( Binja.Llil.startIndex,
    Binja.Llil.sourceFunc,
    Binja.Llil.fromRef,
    Binja.Llil.at,
  )
where

import Binja.BinaryView (functionsContaining)
import Binja.FFI
import Binja.Function
import Binja.Types
import Binja.Utils (ptrToMaybe)

sourceFunc :: BNLlilFunctionPtr -> IO (Maybe BNFunctionPtr)
sourceFunc func = do
  ptrToMaybe <$> c_BNGetLowLevelILOwnderFunction func

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

-- Convert an instruction index into an expression index
instIndexToExprIndex :: BNLlilFunctionPtr -> Word64 -> IO CSize
instIndexToExprIndex = c_BNGetLowLevelILIndexForInstruction

llilByIndex :: BNLlilFunctionPtr -> CSize -> IO BNLowLevelILInstruction
llilByIndex func index =
  alloca $ \p -> do
    _ <- c_BNGetLowLevelILByIndexPtr p func index
    peek p

-- Retrieve the best LLIL instruction for the address in BNReferenceSource
fromRef :: BNReferenceSource -> IO (Maybe BNLowLevelILInstruction)
fromRef ref = do
  func <- Binja.Function.llil (bnFunc ref)
  sIndex <- startIndex func (bnArch ref) (bnAddr ref)
  case sIndex of
    Nothing -> return Nothing
    Just sIndex' -> do
      exprIndex <- instIndexToExprIndex func (fromIntegral sIndex')
      Just <$> llilByIndex func exprIndex

at :: BNBinaryViewPtr -> Word64 -> IO (Maybe BNLowLevelILInstruction)
at view addr = do
  rawFunc <- head <$> functionsContaining view addr
  llilFunc <- Binja.Function.llil rawFunc
  sIndex <- startIndex llilFunc (Binja.Function.architecture rawFunc) addr
  case sIndex of
    Nothing -> return Nothing
    Just sIndex' -> do
      exprIndex <- instIndexToExprIndex llilFunc (fromIntegral sIndex')
      Just <$> llilByIndex llilFunc exprIndex
