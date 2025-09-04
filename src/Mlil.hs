{-# LANGUAGE DuplicateRecordFields #-}
module Mlil
  ( Mlil.fromRef
  , Mlil.instructions
  ) where

import Types
import Function
import BinaryView


foreign import ccall unsafe "BNMediumLevelILGetInstructionStart"
  c_BNMediumLevelILGetInstructionStart
    :: BNMlilFunctionPtr -> BNArchPtr -> Word64 -> IO CSize


foreign import ccall unsafe "BNGetMediumLevelILInstructionCount"
  c_BNGetMediumLevelILInstructionCount
  :: BNMlilFunctionPtr -> IO CSize


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


-- c_BNMediumLevelILGetInstructionStart is not be well defined
-- on mlil ssa function pointer. 
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
    then error ("startIndex: startI:" ++ show startI ++ " >= count:" ++ show count)
    else return startI


-- Convert an instruction index into an expression index
instIndexToExprIndex :: BNMlilFunctionPtr -> Word64 -> IO CSize
instIndexToExprIndex = c_BNGetMediumLevelILIndexForInstruction


mlilSSAByIndex :: BNMlilSSAFunctionPtr -> CSize -> IO BNMediumLevelILInstruction
mlilSSAByIndex func index = do
  alloca $ \p -> do
    _ <- c_BNGetMediumLevelILByIndexPtr p func index
    peek p


-- Given a raw mlil function pointer and expr index valid for mlil (not mlil ssa):
--   (1) cast raw mlil function pointer to raw mlil ssa function pointer
--   (2) cast mlil expression index to mlil ssa expression index
mlilByIndex :: BNMlilFunctionPtr -> CSize -> IO BNMediumLevelILInstruction
mlilByIndex func index = do
  ssaExprIndex <- c_BNGetMediumLevelILSSAExprIndex func index
  ssaFunc <- mlilToSSA func
  alloca $ \p -> do
    _ <- c_BNGetMediumLevelSSAILByIndexPtr p ssaFunc ssaExprIndex
    peek p


-- Retrieve the best MLIL instruction for the address in BNReferenceSource
fromRef :: BNReferenceSource -> IO MediumLevelILSSAInstruction
fromRef ref = do
  -- Get mlil (non-ssa) expression index
  func <- mlil (bnFunc ref)
  sIndex <- startIndex func (bnArch ref) (bnAddr ref)
  exprIndex' <- instIndexToExprIndex func (fromIntegral sIndex)
  -- Convert func and expression index to SSA
  funcSSA <- mlilToSSA func
  ssaExprIndex <- c_BNGetMediumLevelILSSAExprIndex func exprIndex'
  create funcSSA ssaExprIndex


foreign import ccall unsafe "BNMediumLevelILFreeOperandList"
  c_BNMediumLevelILFreeOperandList
    :: Ptr CULLong -> IO ()


foreign import ccall unsafe "BNMediumLevelILGetOperandList"
  c_BNMediumLevelILGetOperandList
    :: BNMlilSSAFunctionPtr -> CSize -> CSize -> Ptr CSize -> IO (Ptr CULLong)


getExprList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [MediumLevelILSSAInstruction]
getExprList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count  <- fromIntegral <$> peek countPtr
    xs <- if rawPtr == nullPtr || count == 0
      then return []
      else peekArray count rawPtr
    when (rawPtr /= nullPtr) $ c_BNMediumLevelILFreeOperandList rawPtr
    mapM (create func . fromIntegral) xs


getExpr :: BNMlilSSAFunctionPtr -> CSize -> IO MediumLevelILSSAInstruction
getExpr = create


getInt :: BNMediumLevelILInstruction -> CSize -> IO Int
getInt inst operand = return $ fromIntegral $ getOp inst operand 


getIntList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [Int]
getIntList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count  <- fromIntegral <$> peek countPtr
    xs <- if rawPtr == nullPtr || count == 0
      then return []
      else peekArray count rawPtr
    when (rawPtr /= nullPtr) $ c_BNMediumLevelILFreeOperandList rawPtr
    return $ map fromIntegral xs


foreign import ccall unsafe "BNFromVariableIdentifierPtr"
  c_BNFromVariableIdentifierPtr
    :: Ptr BNVariable -> CULLong -> IO ()


foreign import ccall unsafe "freeBNVariable"
  c_freeBNVariable
    :: Ptr BNVariable -> IO ()


varFromID :: CULLong -> IO BNVariable
varFromID index =
  alloca $ \p -> do
    _ <- c_BNFromVariableIdentifierPtr p index
    peek p


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


getSSAVar :: BNMediumLevelILInstruction -> CSize -> CSize -> IO BNSSAVariable
getSSAVar inst varOP version' = do
  rawVar <- varFromID $ fromIntegral $ getOp inst varOP
  return $ BNSSAVariable rawVar $ fromIntegral $ getOp inst version'


getSSAVarAndDest :: BNMediumLevelILInstruction -> CSize -> CSize -> IO BNSSAVariable
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
            _ -> error $ "getFloat: " ++ show index ++ " not in [0, .., 4]"


foreign import ccall unsafe "BNGetConstantData"
  c_BNGetConstantData
    :: BNFunctionPtr
    -> Word64
    -> Word64
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
      _ -> error $ "getConstantData: state: " ++ show op1 ++ " not in [0, .., 4]"
    value = case op2 of
      0 -> mlOp0 inst
      1 -> mlOp1 inst
      2 -> mlOp2 inst
      3 -> mlOp3 inst
      4 -> mlOp4 inst
      _ -> error $ "getConstantData: value " ++ show op2 ++ " not in [0, .., 4]"


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
               target' <- peekElemOff rawPtr (j * 2 + 1)
               return (key, target')
    when (rawPtr /= nullPtr) $
      c_BNMediumLevelILFreeOperandList rawPtr
    return pairs


getIntrinsicIL :: BNMediumLevelILInstruction -> BNMlilSSAFunctionPtr -> CSize -> IO ILIntrinsic
getIntrinsicIL inst func operand = do
  let index' = getOp inst operand
  rawFunc <- Function.mlilToRawFunction func
  let arch' = architecture rawFunc
  archTy <- getArch arch'
  intrinsic' <- getIntrinsic archTy index'
  return $ ILIntrinsic index' arch' archTy intrinsic'


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
                                     _ -> error $ "getConstraint: " ++ show operand ++ " not in [0, .., 4]"


foreign import ccall unsafe "BNGetMediumLevelILBasicBlockList"
  c_BNGetMediumLevelILBasicBlockList
  :: BNMlilFunctionPtr -> Ptr CSize -> IO (Ptr BNBasicBlockPtr)


foreign import ccall unsafe "BNFreeBasicBlockList"
  c_BNFreeBasicBlockList
  :: Ptr BNBasicBlockPtr -> CSize -> IO ()


foreign import ccall unsafe "BNGetBasicBlockStart"
  c_BNGetBasicBlockStart
  :: BNBasicBlockPtr -> IO CULLong


foreign import ccall unsafe "BNGetBasicBlockEnd"
  c_BNGetBasicBlockEnd
  :: BNBasicBlockPtr -> IO CULLong


foreign import ccall unsafe "BNGetBasicBlockFunction"
  c_BNGetBasicBlockFunction
  :: BNBasicBlockPtr -> IO BNFunctionPtr


basicBlocks :: BNMlilFunctionPtr -> IO [BNBasicBlockPtr]
basicBlocks func =
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetMediumLevelILBasicBlockList func countPtr
    count  <- peek countPtr
    if arrPtr == nullPtr || count == 0
    then error "basicBlocks: arrPtr null or count is 0"
    else do
      refs <- peekArray (fromIntegral count) (castPtr arrPtr :: Ptr BNBasicBlockPtr)
      c_BNFreeBasicBlockList arrPtr count
      return refs


blockToInstructions :: BNBasicBlockPtr -> IO [MediumLevelILSSAInstruction]
blockToInstructions block = do
  startExpr <- fromIntegral <$> c_BNGetBasicBlockStart block
  endExpr <- fromIntegral <$> c_BNGetBasicBlockEnd block
  func <- c_BNGetBasicBlockFunction block
  mlilFunc <- mlil func
  mlilSSAFunc <- mlilSSA func
  exprs <- mapM (instIndexToExprIndex mlilFunc) [startExpr .. endExpr-1]
  ssaExprs <- mapM (c_BNGetMediumLevelILSSAExprIndex mlilFunc) exprs
  mapM (create mlilSSAFunc) ssaExprs 


instructionsFromFunc :: BNMlilFunctionPtr -> IO [MediumLevelILSSAInstruction]
instructionsFromFunc func = do
  blocks <- basicBlocks func
  perBlock <- mapM blockToInstructions blocks
  return $ concat perBlock


instructions :: BNBinaryViewPtr -> IO [MediumLevelILSSAInstruction]
instructions view = do
  rawFuncs <- BinaryView.functions view
  mlilFuncs <- mapM mlil rawFuncs
  allInsts <- mapM instructionsFromFunc mlilFuncs
  return $ concat allInsts 


data CoreMediumLevelILInstruction = CoreMediumLevelILInstruction
  { instr :: BNMediumLevelILInstruction
  , ilFunc :: BNMlilSSAFunctionPtr
  , exprIndex :: CSize
  } deriving (Show)


data MediumLevelILCallSsaRec = MediumLevelILCallSsaRec
  { output :: [BNSSAVariable]
  , dest :: MediumLevelILSSAInstruction
  , params :: [MediumLevelILSSAInstruction]
  , srcMemory :: Int
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCallOutputSsaRec = MediumLevelILCallOutputSsaRec
  { destMemory :: Int
  , dest :: [BNSSAVariable]
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILConstPtrRec = MediumLevelILConstPtrRec
  { constant :: Int
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILConstRec = MediumLevelILConstRec
  { constant :: Int
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILRetRec = MediumLevelILRetRec
  { src :: [MediumLevelILSSAInstruction]
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILVarSsaRec = MediumLevelILVarSsaRec
  { src :: BNSSAVariable
  , var :: BNSSAVariable
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILSetVarSsaRec = MediumLevelILSetVarSsaRec
  { dest :: BNSSAVariable
  , src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILJumpRec = MediumLevelILJumpRec
  { dest :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILTailcallSsaRec = MediumLevelILTailcallSsaRec
  { output :: [BNSSAVariable]
  , outputDestMemory :: Int
  , dest :: MediumLevelILSSAInstruction
  , params :: [MediumLevelILSSAInstruction]
  , srcMemory :: Int
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILImportRec = MediumLevelILImportRec
  { constant :: Int
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILAddressOfRec = MediumLevelILAddressOfRec
  { src :: BNVariable
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILLoadSsaRec = MediumLevelILLoadSsaRec
  { src :: MediumLevelILSSAInstruction
  , srcMemory :: Int
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILIfRec = MediumLevelILIfRec
  { condition :: MediumLevelILSSAInstruction
  , true :: Int -- Instruction Index
  , false :: Int -- Instruction Index
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCmpERec = MediumLevelILCmpERec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCmpSleRec = MediumLevelILCmpSleRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCmpNeRec = MediumLevelILCmpNeRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCmpSltRec = MediumLevelILCmpSltRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCmpUltRec = MediumLevelILCmpUltRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCmpUleRec = MediumLevelILCmpUleRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCmpSgeRec = MediumLevelILCmpSgeRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCmpUgeRec = MediumLevelILCmpUgeRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCmpSgtRec = MediumLevelILCmpSgtRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCmpUgtRec = MediumLevelILCmpUgtRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILAndRec = MediumLevelILAndRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILOrRec = MediumLevelILOrRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILXorRec = MediumLevelILXorRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILLslRec = MediumLevelILLslRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILLsrRec = MediumLevelILLsrRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILAsrRec = MediumLevelILAsrRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILRolRec = MediumLevelILRolRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILRorRec = MediumLevelILRorRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILMulRec = MediumLevelILMulRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILMuluRec = MediumLevelILMuluRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILMuluDpRec = MediumLevelILMuluDpRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILMulsDpRec = MediumLevelILMulsDpRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILDivsRec = MediumLevelILDivsRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILDivuRec = MediumLevelILDivuRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILDivuDpRec = MediumLevelILDivuDpRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILDivsDpRec = MediumLevelILDivsDpRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILModuRec = MediumLevelILModuRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILModuDpRec = MediumLevelILModuDpRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILModsRec = MediumLevelILModsRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILModsDpRec = MediumLevelILModsDpRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILNoRetRec = MediumLevelILNoRetRec
  { core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILStoreSsaRec = MediumLevelILStoreSsaRec
  { dest :: MediumLevelILSSAInstruction
  , destMemory :: Int
  , srcMemory :: Int
  , src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILSetVarSsaFieldRec = MediumLevelILSetVarSsaFieldRec
  { dest :: BNSSAVariable
  , prev :: BNSSAVariable
  , offset :: Int
  , src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILSetVarAliasedRec = MediumLevelILSetVarAliasedRec
  { dest :: BNSSAVariable
  , prev :: BNSSAVariable
  , src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILVarSsaFieldRec = MediumLevelILVarSsaFieldRec
  { src :: BNSSAVariable
  , offset :: Int
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)
 

data MediumLevelILGotoRec = MediumLevelILGotoRec
  { dest :: Int -- InstructionIndex
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILAddRec = MediumLevelILAddRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILSubRec = MediumLevelILSubRec
  { left :: MediumLevelILSSAInstruction
  , right :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILNegRec = MediumLevelILNegRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILNotRec = MediumLevelILNotRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILSxRec = MediumLevelILSxRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILZxRec = MediumLevelILZxRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILLowPartRec = MediumLevelILLowPartRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILFsqrtRec = MediumLevelILFsqrtRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILFnegRec = MediumLevelILFnegRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILFabsRec = MediumLevelILFabsRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILFloatToIntRec = MediumLevelILFloatToIntRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILIntToFloatRec = MediumLevelILIntToFloatRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILFloatConvRec = MediumLevelILFloatConvRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILRoundToIntRec = MediumLevelILRoundToIntRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILFloorRec = MediumLevelILFloorRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCeilRec = MediumLevelILCeilRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILFtruncRec = MediumLevelILFtruncRec
  { src :: MediumLevelILSSAInstruction
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILJumpToRec = MediumLevelILJumpToRec
  { dest :: MediumLevelILSSAInstruction
  , target :: TargetMap
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILIntrinsicSsaRec = MediumLevelILIntrinsicSsaRec
  { output :: [BNSSAVariable]
  , intrinsic :: ILIntrinsic
  , params :: [MediumLevelILSSAInstruction]
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILSSAInstruction =
   MediumLevelILCallSsa MediumLevelILCallSsaRec
 | MediumLevelILCallOutputSsa MediumLevelILCallOutputSsaRec
 | MediumLevelILConstPtr MediumLevelILConstPtrRec
 | MediumLevelILRet MediumLevelILRetRec
 | MediumLevelILVarSsa MediumLevelILVarSsaRec
 | MediumLevelILSetVarSsa MediumLevelILSetVarSsaRec
 | MediumLevelILJump MediumLevelILJumpRec
 | MediumLevelILJumpTo MediumLevelILJumpToRec
 | MediumLevelILTailcallSsa MediumLevelILTailcallSsaRec
 | MediumLevelILImport MediumLevelILImportRec
 | MediumLevelILAddressOf MediumLevelILAddressOfRec
 | MediumLevelILLoadSsa MediumLevelILLoadSsaRec
 | MediumLevelILConst MediumLevelILConstRec
 | MediumLevelILIf MediumLevelILIfRec
 | MediumLevelILCmpE MediumLevelILCmpERec
 | MediumLevelILCmpNe MediumLevelILCmpNeRec
 | MediumLevelILCmpSle MediumLevelILCmpSleRec
 | MediumLevelILCmpSlt MediumLevelILCmpSltRec
 | MediumLevelILCmpUlt MediumLevelILCmpUltRec
 | MediumLevelILCmpUle MediumLevelILCmpUleRec
 | MediumLevelILCmpSge MediumLevelILCmpSgeRec
 | MediumLevelILCmpUge MediumLevelILCmpUgeRec
 | MediumLevelILCmpSgt MediumLevelILCmpSgtRec
 | MediumLevelILCmpUgt MediumLevelILCmpUgtRec
 | MediumLevelILAnd MediumLevelILAndRec
 | MediumLevelILOr MediumLevelILOrRec
 | MediumLevelILXor MediumLevelILXorRec
 | MediumLevelILLsl MediumLevelILLslRec
 | MediumLevelILLsr MediumLevelILLsrRec
 | MediumLevelILAsr MediumLevelILAsrRec
 | MediumLevelILRol MediumLevelILRolRec
 | MediumLevelILRor MediumLevelILRorRec
 | MediumLevelILMul MediumLevelILMulRec
 | MediumLevelILNoRet MediumLevelILNoRetRec
 | MediumLevelILStoreSsa MediumLevelILStoreSsaRec
 | MediumLevelILSetVarAliased MediumLevelILSetVarAliasedRec
 | MediumLevelILSetVarSsaField MediumLevelILSetVarSsaFieldRec
 | MediumLevelILVarSsaField MediumLevelILVarSsaFieldRec
 | MediumLevelILGoto MediumLevelILGotoRec
 | MediumLevelILAdd MediumLevelILAddRec
 | MediumLevelILSub MediumLevelILSubRec
 | MediumLevelILMuluDp MediumLevelILMuluDpRec
 | MediumLevelILMulsDp MediumLevelILMulsDpRec
 | MediumLevelILDivu MediumLevelILDivuRec
 | MediumLevelILDivuDp MediumLevelILDivuDpRec
 | MediumLevelILDivs MediumLevelILDivsRec
 | MediumLevelILDivsDp MediumLevelILDivsDpRec
 | MediumLevelILModu MediumLevelILModuRec
 | MediumLevelILModuDp MediumLevelILModuDpRec
 | MediumLevelILMods MediumLevelILModsRec
 | MediumLevelILModsDp MediumLevelILModsDpRec
 | MediumLevelILNeg MediumLevelILNegRec
 | MediumLevelILNot MediumLevelILNotRec
 | MediumLevelILCeil MediumLevelILCeilRec
 | MediumLevelILSx MediumLevelILSxRec
 | MediumLevelILZx MediumLevelILZxRec
 | MediumLevelILLowPart MediumLevelILLowPartRec
 | MediumLevelILFsqrt MediumLevelILFsqrtRec
 | MediumLevelILFneg MediumLevelILFnegRec
 | MediumLevelILFabs MediumLevelILFabsRec
 | MediumLevelILFloatToInt MediumLevelILFloatToIntRec
 | MediumLevelILIntToFloat MediumLevelILIntToFloatRec
 | MediumLevelILFloatConv MediumLevelILFloatConvRec
 | MediumLevelILRoundToInt MediumLevelILRoundToIntRec
 | MediumLevelILFloor MediumLevelILFloorRec
 | MediumLevelILFtrunc MediumLevelILFtruncRec
 | MediumLevelILIntrinsicSsa MediumLevelILIntrinsicSsaRec
 deriving (Show)


getOp :: BNMediumLevelILInstruction -> CSize -> CSize
getOp inst operand =
  fromIntegral $ case operand of
                  0 -> mlOp0 inst
                  1 -> mlOp1 inst
                  2 -> mlOp2 inst
                  3 -> mlOp3 inst
                  4 -> mlOp4 inst
                  _ -> error $ "getOp: " ++ show operand ++ " not in [0, .., 4]"


create :: BNMlilSSAFunctionPtr -> CSize -> IO MediumLevelILSSAInstruction
create func exprIndex'  = do
  rawInst <- mlilSSAByIndex func exprIndex'
  let coreInst = CoreMediumLevelILInstruction
             { instr = rawInst
             , ilFunc = func
             , exprIndex = exprIndex'
             }
  case mlOperation rawInst of
    MLIL_NOP -> do
       error $ ("Unimplemented: " ++ show "MLIL_NOP")
    MLIL_SET_VAR -> do
       error $ ("Unimplemented: " ++ show "MLIL_SET_VAR")
    MLIL_SET_VAR_FIELD -> do
       error $ ("Unimplemented: " ++ show "MLIL_SET_VAR_FIELD")
    MLIL_SET_VAR_SPLIT -> do
       error $ ("Unimplemented: " ++ show "MLIL_SET_VAR_SPLIT")
    MLIL_ASSERT -> do
       error $ ("Unimplemented: " ++ show "MLIL_ASSERT")
    MLIL_FORCE_VER -> do
       error $ ("Unimplemented: " ++ show "MLIL_FORCE_VER")
    MLIL_LOAD -> do
       error $ ("Unimplemented: " ++ show "MLIL_LOAD")
    MLIL_LOAD_STRUCT -> do
       error $ ("Unimplemented: " ++ show "MLIL_LOAD_STRUCT")
    MLIL_STORE -> do
       error $ ("Unimplemented: " ++ show "MLIL_STORE")
    MLIL_STORE_STRUCT -> do
       error $ ("Unimplemented: " ++ show "MLIL_STORE_STRUCT")
    MLIL_VAR -> do
       error $ ("Unimplemented: " ++ show "MLIL_VAR")
    MLIL_VAR_FIELD -> do
       error $ ("Unimplemented: " ++ show "MLIL_VAR_FIELD")
    MLIL_VAR_SPLIT -> do
       error $ ("Unimplemented: " ++ show "MLIL_VAR_SPLIT")
    MLIL_ADDRESS_OF -> do
      src' <- varFromID $ fromIntegral $ getOp rawInst 0
      let rec = MediumLevelILAddressOfRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILAddressOf rec
    MLIL_ADDRESS_OF_FIELD -> do
       error $ ("Unimplemented: " ++ show "MLIL_ADDRESS_OF_FIELD")
    MLIL_CONST -> do
      constant' <- getInt rawInst 0
      let rec = MediumLevelILConstRec
                { constant = constant'
                , core = coreInst
                }
      return $ MediumLevelILConst rec
    MLIL_CONST_DATA -> do
       error $ ("Unimplemented: " ++ show "MLIL_CONST_DATA")
    MLIL_CONST_PTR -> do
      constant' <- getInt rawInst 0
      let rec = MediumLevelILConstPtrRec
                { constant = constant'
                , core = coreInst
                }
      return $ MediumLevelILConstPtr rec
    MLIL_EXTERN_PTR -> do
       error $ ("Unimplemented: " ++ show "MLIL_EXTERN_PTR")
    MLIL_FLOAT_CONST -> do
       error $ ("Unimplemented: " ++ show "MLIL_FLOAT_CONST")
    MLIL_IMPORT -> do
      constant' <- getInt rawInst 0
      let rec = MediumLevelILImportRec
                { constant = constant'
                , core = coreInst
                }
      return $ MediumLevelILImport rec
    MLIL_ADD -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILAddRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILAdd rec
    MLIL_ADC -> do
       error $ ("Unimplemented: " ++ show "MLIL_ADC")
    MLIL_SUB -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILSubRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILSub rec 
    MLIL_SBB -> do
       error $ ("Unimplemented: " ++ show "MLIL_SBB")
    MLIL_AND -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILAndRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILAnd rec
    MLIL_OR -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILOrRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILOr rec
    MLIL_XOR -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILXorRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILXor rec
    MLIL_LSL -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILLslRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILLsl rec
    MLIL_LSR -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILLsrRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILLsr rec
    MLIL_ASR -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILAsrRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILAsr rec
    MLIL_ROL -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILRolRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILRol rec
    MLIL_RLC -> do
       error $ ("Unimplemented: " ++ show "MLIL_RLC")
    MLIL_ROR -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILRorRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILRor rec
    MLIL_RRC -> do
       error $ ("Unimplemented: " ++ show "MLIL_RRC")
    MLIL_MUL -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILMulRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILMul rec
    MLIL_MULU_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILMuluDpRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILMuluDp rec
    MLIL_MULS_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILMulsDpRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILMulsDp rec
    MLIL_DIVU -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILDivuRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILDivu rec
    MLIL_DIVU_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILDivuDpRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILDivuDp rec
    MLIL_DIVS -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILDivsRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILDivs rec
    MLIL_DIVS_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILDivsDpRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILDivsDp rec
    MLIL_MODU -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILModuRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILModu rec
    MLIL_MODU_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILModuDpRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILModuDp rec
    MLIL_MODS -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILModsRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILMods rec
    MLIL_MODS_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILModsDpRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILModsDp rec
    MLIL_NEG -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILNegRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILNeg rec
    MLIL_NOT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILNotRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILNot rec
    MLIL_SX -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILSxRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILSx rec
    MLIL_ZX -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILZxRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILZx rec
    MLIL_LOW_PART -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILLowPartRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILLowPart rec
    MLIL_JUMP -> do
      dest' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILJumpRec
                { dest = dest'
                , core = coreInst
                }
      return $ MediumLevelILJump rec
    MLIL_JUMP_TO -> do
      dest' <- getExpr func $ getOp rawInst 0
      target' <- getTargetMap func exprIndex' 1
      let rec = MediumLevelILJumpToRec
                { dest = dest'
                , target = target'
                , core = coreInst
                }
      return $ MediumLevelILJumpTo rec
    MLIL_RET_HINT -> do
       error $ ("Unimplemented: " ++ show "MLIL_RET_HINT")
    MLIL_CALL -> do
       error $ ("Unimplemented: " ++ show "MLIL_CALL")
    MLIL_CALL_UNTYPED -> do
       error $ ("Unimplemented: " ++ show "MLIL_CALL_UNTYPED")
    MLIL_CALL_OUTPUT -> do
       error $ ("Unimplemented: " ++ show "MLIL_CALL_OUTPUT")
    MLIL_CALL_PARAM -> do
       error $ ("Unimplemented: " ++ show "MLIL_CALL_PARAM")
    MLIL_SEPARATE_PARAM_LIST -> do
       error $ ("Unimplemented: " ++ show "MLIL_SEPARATE_PARAM_LIST")
    MLIL_SHARED_PARAM_SLOT -> do
       error $ ("Unimplemented: " ++ show "MLIL_SHARED_PARAM_SLOT")
    MLIL_RET -> do
      src' <- getExprList func exprIndex' 0
      let rec = MediumLevelILRetRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILRet rec
    MLIL_NORET -> do
      return $ MediumLevelILNoRet $ MediumLevelILNoRetRec { core = coreInst }
    MLIL_IF -> do
      condition' <- getExpr func $ getOp rawInst 0
      true' <- getInt rawInst 1
      false' <- getInt rawInst 2
      let rec = MediumLevelILIfRec
                { condition = condition'
                , true = true'
                , false = false'
                , core = coreInst
                }
      return $ MediumLevelILIf rec
    MLIL_GOTO -> do
      dest' <- getInt rawInst 0
      let rec = MediumLevelILGotoRec
                { dest = dest'
                , core = coreInst
                }
      return $ MediumLevelILGoto rec
    MLIL_CMP_E -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILCmpERec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILCmpE rec
    MLIL_CMP_NE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILCmpNeRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILCmpNe rec
    MLIL_CMP_SLT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILCmpSltRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILCmpSlt rec
    MLIL_CMP_ULT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILCmpUltRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILCmpUlt rec
    MLIL_CMP_SLE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILCmpSleRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILCmpSle rec
    MLIL_CMP_ULE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILCmpUleRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILCmpUle rec
    MLIL_CMP_SGE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILCmpSgeRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILCmpSge rec
    MLIL_CMP_UGE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILCmpUgeRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILCmpUge rec
    MLIL_CMP_SGT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILCmpSgtRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILCmpSgt rec
    MLIL_CMP_UGT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec = MediumLevelILCmpUgtRec
                { left = left'
                , right = right'
                , core = coreInst
                }
      return $ MediumLevelILCmpUgt rec
    MLIL_TEST_BIT -> do
       error $ ("Unimplemented: " ++ show "MLIL_TEST_BIT")
    MLIL_BOOL_TO_INT -> do
       error $ ("Unimplemented: " ++ show "MLIL_BOOL_TO_INT")
    MLIL_ADD_OVERFLOW -> do
       error $ ("Unimplemented: " ++ show "MLIL_ADD_OVERFLOW")
    MLIL_SYSCALL -> do
       error $ ("Unimplemented: " ++ show "MLIL_SYSCALL")
    MLIL_SYSCALL_UNTYPED -> do
       error $ ("Unimplemented: " ++ show "MLIL_SYSCALL_UNTYPED")
    MLIL_TAILCALL -> do
       error $ ("Unimplemented: " ++ show "MLIL_TAILCALL")
    MLIL_TAILCALL_UNTYPED -> do
       error $ ("Unimplemented: " ++ show "MLIL_TAILCALL_UNTYPED")
    MLIL_INTRINSIC -> do
       error $ ("Unimplemented: " ++ show "MLIL_INTRINSIC")
    MLIL_FREE_VAR_SLOT -> do
       error $ ("Unimplemented: " ++ show "MLIL_FREE_VAR_SLOT")
    MLIL_BP -> do
       error $ ("Unimplemented: " ++ show "MLIL_BP")
    MLIL_TRAP -> do
       error $ ("Unimplemented: " ++ show "MLIL_TRAP")
    MLIL_UNDEF -> do
       error $ ("Unimplemented: " ++ show "MLIL_UNDEF")
    MLIL_UNIMPL -> do
       error $ ("Unimplemented: " ++ show "MLIL_UNIMPL")
    MLIL_UNIMPL_MEM -> do
       error $ ("Unimplemented: " ++ show "MLIL_UNIMPL_MEM")
    MLIL_FADD -> do
       error $ ("Unimplemented: " ++ show "MLIL_FADD")
    MLIL_FSUB -> do
       error $ ("Unimplemented: " ++ show "MLIL_FSUB")
    MLIL_FMUL -> do
       error $ ("Unimplemented: " ++ show "MLIL_FMUL")
    MLIL_FDIV -> do
       error $ ("Unimplemented: " ++ show "MLIL_FDIV")
    MLIL_FSQRT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILFsqrtRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILFsqrt rec
    MLIL_FNEG -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILFnegRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILFneg rec
    MLIL_FABS -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILFabsRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILFabs rec
    MLIL_FLOAT_TO_INT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILFloatToIntRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILFloatToInt rec
    MLIL_INT_TO_FLOAT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILIntToFloatRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILIntToFloat rec
    MLIL_FLOAT_CONV -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILFloatConvRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILFloatConv rec
    MLIL_ROUND_TO_INT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILRoundToIntRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILRoundToInt rec
    MLIL_FLOOR -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILFloorRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILFloor rec
    MLIL_CEIL -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILCeilRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILCeil rec
    MLIL_FTRUNC -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec = MediumLevelILFtruncRec
                { src = src'
                , core = coreInst
                }
      return $ MediumLevelILFtrunc rec
    MLIL_FCMP_E -> do
       error $ ("Unimplemented: " ++ show "MLIL_FCMP_E")
    MLIL_FCMP_NE -> do
       error $ ("Unimplemented: " ++ show "MLIL_FCMP_NE")
    MLIL_FCMP_LT -> do
       error $ ("Unimplemented: " ++ show "MLIL_FCMP_LT")
    MLIL_FCMP_LE -> do
       error $ ("Unimplemented: " ++ show "MLIL_FCMP_LE")
    MLIL_FCMP_GE -> do
       error $ ("Unimplemented: " ++ show "MLIL_FCMP_GE")
    MLIL_FCMP_GT -> do
       error $ ("Unimplemented: " ++ show "MLIL_FCMP_GT")
    MLIL_FCMP_O -> do
       error $ ("Unimplemented: " ++ show "MLIL_FCMP_O")
    MLIL_FCMP_UO -> do
       error $ ("Unimplemented: " ++ show "MLIL_FCMP_UO")
    MLIL_SET_VAR_SSA -> do
      dest' <- getSSAVar rawInst 0 1
      src' <- getExpr func $ getOp rawInst 2
      let rec = MediumLevelILSetVarSsaRec
                { dest=dest'
                , src=src'
                , core=coreInst
                }
      return $ MediumLevelILSetVarSsa rec
    MLIL_SET_VAR_SSA_FIELD -> do
      dest' <- getSSAVarAndDest rawInst 0 1
      prev' <- getSSAVarAndDest rawInst 0 2
      offset' <- getInt rawInst 3
      src' <- getExpr func $ getOp rawInst 4
      let rec = MediumLevelILSetVarSsaFieldRec
                { dest = dest'
                , prev = prev'
                , offset = offset'
                , src = src'
                , core = coreInst
                }
      return $ MediumLevelILSetVarSsaField rec
    MLIL_SET_VAR_SPLIT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_SET_VAR_SPLIT_SSA")
    MLIL_SET_VAR_ALIASED -> do
      dest' <- getSSAVarAndDest rawInst 0 1
      prev' <- getSSAVarAndDest rawInst 0 2
      src' <- getExpr func $ getOp rawInst 3
      let rec = MediumLevelILSetVarAliasedRec
                { dest = dest'
                , prev = prev'
                , src = src'
                , core = coreInst
                }
      return $ MediumLevelILSetVarAliased rec
    MLIL_SET_VAR_ALIASED_FIELD -> do
       error $ ("Unimplemented: " ++ show "MLIL_SET_VAR_ALIASED_FIELD")
    MLIL_VAR_SSA -> do
      src' <- getSSAVar rawInst 0 1
      let rec = MediumLevelILVarSsaRec
                { src=src'
                , var=src'
                , core=coreInst
                }
      return $ MediumLevelILVarSsa rec
    MLIL_VAR_SSA_FIELD -> do
      src' <- getSSAVar rawInst 0 1
      offset' <- getInt rawInst 2
      let rec = MediumLevelILVarSsaFieldRec
                { src = src'
                , offset = offset'
                , core = coreInst 
                }
      return $ MediumLevelILVarSsaField rec
    MLIL_VAR_ALIASED -> do
       error $ ("Unimplemented: " ++ show "MLIL_VAR_ALIASED")
    MLIL_VAR_ALIASED_FIELD -> do
       error $ ("Unimplemented: " ++ show "MLIL_VAR_ALIASED_FIELD")
    MLIL_VAR_SPLIT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_VAR_SPLIT_SSA")
    MLIL_ASSERT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_ASSERT_SSA")
    MLIL_FORCE_VER_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_FORCE_VER_SSA")
    MLIL_CALL_SSA -> do
      outputInst <- getExpr func $ getOp rawInst 0
      output' <- case outputInst of
                MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec{ dest = d }) -> return d
                _ -> error $
                  "create: Output of MediumLevelILCallSsa: expected MediumLevelILCallOutputSsa : "
                  ++ show outputInst
      dest' <- getExpr func $ getOp rawInst 1
      params' <- getExprList func exprIndex' 2
      srcMemory' <- getInt rawInst 4
      let rec = MediumLevelILCallSsaRec
                { output = output'
                , dest = dest'
                , params = params'
                , srcMemory = srcMemory'
                , core = coreInst
                }
      return $ MediumLevelILCallSsa rec
    MLIL_CALL_UNTYPED_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_CALL_UNTYPED_SSA")
    MLIL_SYSCALL_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_SYSCALL_SSA")
    MLIL_SYSCALL_UNTYPED_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_SYSCALL_UNTYPED_SSA")
    MLIL_TAILCALL_SSA -> do
      outputInst <- getExpr func $ getOp rawInst 0
      output' <- case outputInst of
                MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec{ dest = d }) -> return d
                _ -> error $
                  "create: Output of MediumLevelILTailcallSsa: expected MediumLevelILCallOutputSsa : "
                  ++ show outputInst
      outputDestMemory' <- case outputInst of
                MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec{ destMemory = dM }) -> return dM
                _ -> error $
                  "create: Output of MediumLevelILTailcallSsa: expected MediumLevelILCallOutputSsa : "
                  ++ show outputInst
      dest' <- getExpr func $ getOp rawInst 1
      params' <- getExprList func exprIndex' 2
      srcMemory' <- getInt rawInst 4
      let rec = MediumLevelILTailcallSsaRec
                { output = output'
                , outputDestMemory = outputDestMemory' 
                , dest = dest'
                , params = params'
                , srcMemory = srcMemory'
                , core = coreInst
                }
      return $ MediumLevelILTailcallSsa rec
    MLIL_TAILCALL_UNTYPED_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_TAILCALL_UNTYPED_SSA")
    MLIL_CALL_PARAM_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_CALL_PARAM_SSA")
    MLIL_CALL_OUTPUT_SSA -> do
      destMemory' <- getInt rawInst 0
      dest' <- getSSAVarList func exprIndex' 1
      let rec = MediumLevelILCallOutputSsaRec
                { destMemory = destMemory'
                , dest = dest'
                , core = coreInst
                }
      return $ MediumLevelILCallOutputSsa rec
    MLIL_MEMORY_INTRINSIC_OUTPUT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_MEMORY_INTRINSIC_OUTPUT_SSA")
    MLIL_LOAD_SSA -> do
      src' <- getExpr func $ getOp rawInst 0
      srcMemory' <- getInt rawInst 1
      let rec = MediumLevelILLoadSsaRec
                { src=src'
                , srcMemory=srcMemory'
                , core=coreInst
                }
      return $ MediumLevelILLoadSsa rec
    MLIL_LOAD_STRUCT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_LOAD_STRUCT_SSA")
    MLIL_STORE_SSA -> do
      dest' <- getExpr func $ getOp rawInst 0
      destMemory' <- getInt rawInst 1
      srcMemory' <- getInt rawInst 2
      src' <- getExpr func $ getOp rawInst 3
      let rec = MediumLevelILStoreSsaRec
                { dest = dest'
                , destMemory = destMemory'
                , srcMemory = srcMemory'
                , src = src'
                , core = coreInst
                }
      return $ MediumLevelILStoreSsa rec
    MLIL_STORE_STRUCT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_STORE_STRUCT_SSA")
    MLIL_INTRINSIC_SSA -> do
      output' <- getSSAVarList func exprIndex' 0
      intrinsic' <- getIntrinsicIL rawInst func 2
      params' <- getExprList func exprIndex' 3
      let rec = MediumLevelILIntrinsicSsaRec
                { output = output'
                , intrinsic = intrinsic'
                , params = params'
                , core = coreInst
                }
      Prelude.print rec
      return $ MediumLevelILIntrinsicSsa rec 
    MLIL_MEMORY_INTRINSIC_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_MEMORY_INTRINSIC_SSA")
    MLIL_FREE_VAR_SLOT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_FREE_VAR_SLOT_SSA")
    MLIL_VAR_PHI -> do
       error $ ("Unimplemented: " ++ show "MLIL_VAR_PHI")
    MLIL_MEM_PHI -> do
       error $ ("Unimplemented: " ++ show "MLIL_MEM_PHI")
    _ -> error $ "Unknown instruction type: " ++ show rawInst

