{-# LANGUAGE DuplicateRecordFields #-}
module Mlil
  ( Mlil.fromRef
  , Mlil.instructions
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
    :: Ptr BNMediumLevelILInstruction -> BNMlilSSAFunctionPtr -> CSize -> IO (Ptr BNMediumLevelILInstruction)


foreign import ccall unsafe "BNGetMediumLevelSSAILByIndexPtr"
  c_BNGetMediumLevelSSAILByIndexPtr
    :: Ptr BNMediumLevelILInstruction -> BNMlilSSAFunctionPtr -> CSize -> IO (Ptr BNMediumLevelILInstruction)

foreign import ccall unsafe "BNGetMediumLevelILSSAExprIndex"
  c_BNGetMediumLevelILSSAExprIndex
    :: BNMlilFunctionPtr -> CSize -> IO CSize


startIndex :: BNMlilFunctionPtr -> BNArchPtr -> Word64 -> IO CSize
startIndex func arch addr = do
  if arch == nullPtr || func == nullPtr 
  then error "startIndex: called with nullPtr argument"
  else do
    startI <- c_BNMediumLevelILGetInstructionStart func arch addr        
    count <- c_BNGetMediumLevelILInstructionCount func
    -- Ensure start index is less than total mlil instructions
    -- in function
    if startI >= count
    then error "startIndex: startI >= count"
    else return startI


-- Convert an instruction index into an expression index
instIndexToExprIndex :: BNMlilFunctionPtr -> Word64 -> IO CSize
instIndexToExprIndex = c_BNGetMediumLevelILIndexForInstruction


mlilByIndex :: BNMlilSSAFunctionPtr -> CSize -> IO BNMediumLevelILInstruction
mlilByIndex func index = do
  alloca $ \p -> do
    _ <- c_BNGetMediumLevelILByIndexPtr p func index
    peek p


-- Given a raw mlil function pointer and expr index valid for mlil (not mlil ssa):
--   (1) cast raw mlil function pointer to raw mlil ssa function pointer
--   (2) cast mlil expression index to mlil ssa expression index
mlilSSAByIndex :: BNMlilFunctionPtr -> CSize -> IO BNMediumLevelILInstruction
mlilSSAByIndex func index = do
  ssaExprIndex <- c_BNGetMediumLevelILSSAExprIndex func index
  ssaFunc <- mlilToSSA func
  alloca $ \p -> do
    _ <- c_BNGetMediumLevelSSAILByIndexPtr p ssaFunc ssaExprIndex
    if p == nullPtr
      then error "mlilSSAByIndex: c_BNGetMediumLevelSSAILByIndexPtr failed"
      else peek p


-- Retrieve the best MLIL instruction for the address in BNReferenceSource
fromRef :: BNReferenceSource -> IO BNMediumLevelILInstruction
fromRef ref = do
  func <- mlil (bnFunc ref)
  sIndex <- startIndex func (bnArch ref) (bnAddr ref)
  exprIndex <- instIndexToExprIndex func (fromIntegral sIndex)
  mlilSSAByIndex func exprIndex


foreign import ccall unsafe "BNMediumLevelILFreeOperandList"
  c_BNMediumLevelILFreeOperandList
    :: Ptr CULLong -> IO ()


-- assume (delete after check): return is list of expression index to recover list of mlil instructions
foreign import ccall unsafe "BNMediumLevelILGetOperandList"
  c_BNMediumLevelILGetOperandList
    :: BNMlilSSAFunctionPtr -> CSize -> CSize -> Ptr CSize -> IO (Ptr CULLong)


getExprList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [Int]
getExprList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count  <- fromIntegral <$> peek countPtr
    xs <- if rawPtr == nullPtr || count == 0
      then return []
      else map fromIntegral <$> peekArray count rawPtr
    when (rawPtr /= nullPtr) $ c_BNMediumLevelILFreeOperandList rawPtr
    return xs


getExpr :: BNMlilSSAFunctionPtr -> CSize -> IO BNMediumLevelILInstruction
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


getIntList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [Int]
getIntList = getExprList


getInstList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [BNMediumLevelILInstruction]
getInstList func expr operand = do
  indexList <- getExprList func expr operand
  mapM (mlilByIndex func . fromIntegral) indexList


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


getVarList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [BNVariable]
getVarList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count  <- fromIntegral <$> peek countPtr
    xs <- if rawPtr == nullPtr || count == 0
      then return []
      else peekArray count rawPtr
    when (rawPtr /= nullPtr) $ c_BNMediumLevelILFreeOperandList rawPtr
    mapM varFromID xs


getSSAVarList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [BNSSAVariable]
getSSAVarList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count  <- fromIntegral <$> peek countPtr :: IO Int
    let pairCount = count `div` 2
    result <-
      if rawPtr == nullPtr || pairCount == 0
        then return []
        else forM [0 .. pairCount - 1] $ \j -> do
               varId <- peekElemOff rawPtr (j * 2)
               ver   <- peekElemOff rawPtr (j * 2 + 1)
               v     <- varFromID varId
               return (BNSSAVariable v (fromIntegral ver))
    when (rawPtr /= nullPtr) $
      c_BNMediumLevelILFreeOperandList rawPtr
    return result


getVar :: BNMediumLevelILInstruction -> Int -> IO BNVariable
getVar inst index = varFromID value
  where
  value = case index of
          0 -> mlOp0 inst
          1 -> mlOp1 inst
          2 -> mlOp2 inst
          3 -> mlOp3 inst
          4 -> mlOp4 inst


getSSAVar :: BNMediumLevelILInstruction -> Int -> Int -> IO BNSSAVariable
getSSAVar inst indexVar indexVersion = do
  rawVar <- getVar inst indexVar
  return $ BNSSAVariable rawVar version
  where
    version = fromIntegral $ case indexVersion of
                             0 -> mlOp0 inst
                             1 -> mlOp1 inst
                             2 -> mlOp2 inst
                             3 -> mlOp3 inst
                             4 -> mlOp4 inst


getSSAVarAndDest :: BNMediumLevelILInstruction -> Int -> Int -> IO BNSSAVariable
getSSAVarAndDest = getSSAVar 


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


getTargetMap :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO TargetMap
getTargetMap func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count  <- fromIntegral <$> peek countPtr :: IO Int
    let pairCount = count `div` 2
    pairs <-
      if rawPtr == nullPtr || pairCount == 0
        then return []
        else forM [0 .. pairCount - 1] $ \j -> do
               key    <- peekElemOff rawPtr (j * 2)
               target <- peekElemOff rawPtr (j * 2 + 1)
               return (key, target)
    when (rawPtr /= nullPtr) $
      c_BNMediumLevelILFreeOperandList rawPtr
    return pairs


getIntrinsic :: BNMediumLevelILInstruction -> BNArchPtr -> Int -> IO ILIntrinsic
getIntrinsic inst arch operand = return $ ILIntrinsic intrinsicIndex arch
  where
    intrinsicIndex = case operand of
                     0 -> mlOp0 inst
                     1 -> mlOp1 inst
                     2 -> mlOp2 inst
                     3 -> mlOp3 inst
                     4 -> mlOp4 inst


foreign import ccall unsafe "BNGetCachedMediumLevelILPossibleValueSetPtr"
  c_BNGetCachedMediumLevelILPossibleValueSetPtr
  :: BNMlilSSAFunctionPtr -> CSize -> IO (Ptr BNPossibleValueSet)

foreign import ccall unsafe "freeBNPossibleValueSet"
  c_freeBNPossibleValueSet :: (Ptr BNPossibleValueSet) -> IO ()


getConstraint :: BNMlilSSAFunctionPtr -> BNMediumLevelILInstruction -> CSize -> IO BNPossibleValueSet
getConstraint func inst operand = do
  possibleValuePtr <- c_BNGetCachedMediumLevelILPossibleValueSetPtr func constraintIndex
  possibleValue <- peek possibleValuePtr
  c_freeBNPossibleValueSet possibleValuePtr
  return possibleValue
  where
    constraintIndex = fromIntegral $ case operand of
                                     0 -> mlOp0 inst
                                     1 -> mlOp1 inst
                                     2 -> mlOp2 inst
                                     3 -> mlOp3 inst
                                     4 -> mlOp4 inst


foreign import ccall unsafe "BNGetMediumLevelILBasicBlockList"
  c_BNGetMediumLevelILBasicBlockList
  :: BNMlilSSAFunctionPtr -> Ptr CSize -> IO (Ptr BNBasicBlockPtr)


foreign import ccall unsafe "BNFreeBasicBlockList"
  c_BNFreeBasicBlockList
  :: Ptr BNBasicBlockPtr -> CSize -> IO ()


foreign import ccall unsafe "BNGetBasicBlockStart"
  c_BNGetBasicBlockStart
  :: BNBasicBlockPtr -> IO CULLong


foreign import ccall unsafe "BNGetBasicBlockEnd"
  c_BNGetBasicBlockEnd
  :: BNBasicBlockPtr -> IO CULLong


foreign import ccall unsafe "BNGetBasicBlockLength"
  c_BNGetBasicBlockLength
  :: BNBasicBlockPtr -> IO CULLong

foreign import ccall unsafe "BNGetBasicBlockFunction"
  c_BNGetBasicBlockFunction
  :: BNBasicBlockPtr -> IO BNFunctionPtr


basicBlocks :: BNMlilSSAFunctionPtr -> IO [BNBasicBlockPtr]
basicBlocks func =
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetMediumLevelILBasicBlockList func countPtr
    count  <- peek countPtr
    if arrPtr == nullPtr || count == 0
    then return []
    else do
      refs <- peekArray (fromIntegral count) (castPtr arrPtr :: Ptr BNBasicBlockPtr)
      c_BNFreeBasicBlockList arrPtr count
      return refs


blockToInstructions :: BNBasicBlockPtr -> IO [BNMediumLevelILInstruction]
blockToInstructions block = do
  startExpr <- fromIntegral <$> c_BNGetBasicBlockStart block
  endExpr <- fromIntegral <$> c_BNGetBasicBlockEnd block
  func <- c_BNGetBasicBlockFunction block
  mlilFunc <- mlil func
  mapM (mlilSSAByIndex mlilFunc) [startExpr .. endExpr-1]


instructions :: BNMlilSSAFunctionPtr -> IO [BNMediumLevelILInstruction]
instructions func = do
  blocks <- basicBlocks func
  perBlock <- mapM blockToInstructions blocks
  return $ concat perBlock


data MediumLevelILSSAInstruction =
  MediumLevelILCallSsa
  { output :: [BNSSAVariable]
  , dest :: MediumLevelILSSAInstruction
  , params :: [MediumLevelILSSAInstruction]
  , srcMem :: Int
  , instr :: BNMediumLevelILInstruction
  }


--create :: BNMlilFunctionPtr -> CSize -> MediumLevelILSSAInstruction
--create func exprIndex  = do


