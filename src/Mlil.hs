module Mlil
  ( Mlil.fromRef
  ) where

import Types
import ReferenceSource
import Function
import GHC.Float (float2Double)


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


foreign import ccall unsafe "BNMediumLevelILFreeOperandList"
  c_BNMediumLevelILFreeOperandList
    :: Ptr CULLong -> IO ()


-- assume (delete after check): return is list of expression index to recover list of mlil instructions
foreign import ccall unsafe "BNMediumLevelILGetOperandList"
  c_BNMediumLevelILGetOperandList
    :: BNMlilFunctionPtr -> CSize -> CSize -> Ptr CSize -> IO (Ptr CULLong)


getExprList :: BNMlilFunctionPtr -> CSize -> CSize -> IO [Maybe BNMediumLevelILInstruction]
getExprList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count  <- fromIntegral <$> peek countPtr
    xs <- if rawPtr == nullPtr || count == 0
      then return []
      else peekArray count rawPtr
    when (rawPtr /= nullPtr) $ c_BNMediumLevelILFreeOperandList rawPtr
    mapM (mlilByIndex func . fromIntegral ) xs


getExpr :: BNMlilFunctionPtr -> CSize -> IO (Maybe BNMediumLevelILInstruction)
getExpr = mlilByIndex 


getInt :: BNMediumLevelILInstruction -> Int -> Int
getInt inst index =
  fromIntegral (fromIntegral value :: Int64)
  where
  value = case index of
          0 -> mlOp0 inst
          1 -> mlOp1 inst
          2 -> mlOp2 inst
          3 -> mlOp3 inst
          4 -> mlOp4 inst


foreign import ccall unsafe "BNFromVariableIdentifierPtr"
  c_BNFromVariableIdentifierPtr
    :: CULLong -> IO (Ptr BNVariable)

foreign import ccall unsafe "freeBNVariable"
  c_freeBNVariable
    :: Ptr BNVariable -> IO ()


varFromID :: CULLong -> IO BNVariable
varFromID index = do
  rawPtr <- c_BNFromVariableIdentifierPtr index
  val <- peek rawPtr
  c_freeBNVariable rawPtr
  return val


getVarList :: BNMlilFunctionPtr -> CSize -> CSize -> IO [BNVariable]
getVarList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count  <- fromIntegral <$> peek countPtr
    xs <- if rawPtr == nullPtr || count == 0
      then return []
      else peekArray count rawPtr
    when (rawPtr /= nullPtr) $ c_BNMediumLevelILFreeOperandList rawPtr
    mapM varFromID xs

 
getVar :: BNMediumLevelILInstruction -> Int -> IO BNVariable
getVar inst index = varFromID value
  where
  value = case index of
          0 -> mlOp0 inst
          1 -> mlOp1 inst
          2 -> mlOp2 inst
          3 -> mlOp3 inst
          4 -> mlOp4 inst


getFloat :: BNMediumLevelILInstruction -> Int -> Double
getFloat inst index =
  case mlSize inst of
    4 -> float2Double $ castWord32ToFloat w32
    8 -> castWord64ToDouble w64
    _ -> fromIntegral value
  where
    w64 = fromIntegral value :: Word64
    w32 = fromIntegral $ w64 .&. 0xffffffff :: Word32
    value = case index of
            0 -> mlOp0 inst
            1 -> mlOp1 inst
            2 -> mlOp2 inst
            3 -> mlOp3 inst
            4 -> mlOp4 inst


foreign import ccall unsafe "BNGetConstantData"
  c_BNGetConstantData
    :: BNFunctionPtr
    -> CULLong
    -> CULLong
    -> CSize
    -> Ptr CInt
    -> IO BNDataBufferPtr


getConstantData :: BNFunctionPtr -> BNMediumLevelILInstruction -> Int -> Int -> IO BNDataBufferPtr
getConstantData func inst op1 op2 =
  c_BNGetConstantData func state value (mlSize inst) nullPtr 
  where
    state = case op1 of
      0 -> mlOp0 inst
      1 -> mlOp1 inst
      2 -> mlOp2 inst
      3 -> mlOp3 inst
      4 -> mlOp4 inst
    value = case op2 of
      0 -> mlOp0 inst
      1 -> mlOp1 inst
      2 -> mlOp2 inst
      3 -> mlOp3 inst
      4 -> mlOp4 inst



