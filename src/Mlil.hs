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


--getSSAVarAndDest :: BNMediumLevelILInstruction -> CSize -> CSize -> IO BNSSAVariable
--getSSAVarAndDest = getSSAVar 


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
                     _ -> error $ "getIntrinsic: " ++ show operand ++ " not in [0, .., 4]"


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
  , srcMem :: Int
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILCallOutputSsaRec = MediumLevelILCallOutputSsaRec
  { destMemory :: Int
  , dest :: [BNSSAVariable]
  , core :: CoreMediumLevelILInstruction
  } deriving (Show)


data MediumLevelILConstPtrRec = MediumLevelILConstPtrRec
  { constant :: Int
  } deriving (Show)


data MediumLevelILRetRec = MediumLevelILRetRec
  { src :: [MediumLevelILSSAInstruction]
  } deriving (Show)


data MediumLevelILVarSsaRec = MediumLevelILVarSsaRec
  { src :: BNSSAVariable
  , var :: BNSSAVariable
  } deriving (Show)



data MediumLevelILSSAInstruction =
   MediumLevelILCallSsa MediumLevelILCallSsaRec
 | MediumLevelILCallOutputSsa MediumLevelILCallOutputSsaRec
 | MediumLevelILConstPtr MediumLevelILConstPtrRec
 | MediumLevelILRet MediumLevelILRetRec
 | MediumLevelILVarSsa MediumLevelILVarSsaRec
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
       error $ ("Unimplemented: " ++ show "MLIL_ADDRESS_OF")
    MLIL_ADDRESS_OF_FIELD -> do
       error $ ("Unimplemented: " ++ show "MLIL_ADDRESS_OF_FIELD")
    MLIL_CONST -> do
       error $ ("Unimplemented: " ++ show "MLIL_CONST")
    MLIL_CONST_DATA -> do
       error $ ("Unimplemented: " ++ show "MLIL_CONST_DATA")
    MLIL_CONST_PTR -> do
      let rec = MediumLevelILConstPtrRec { constant = fromIntegral $ getOp rawInst 0 }
      return $ MediumLevelILConstPtr rec
    MLIL_EXTERN_PTR -> do
       error $ ("Unimplemented: " ++ show "MLIL_EXTERN_PTR")
    MLIL_FLOAT_CONST -> do
       error $ ("Unimplemented: " ++ show "MLIL_FLOAT_CONST")
    MLIL_IMPORT -> do
       error $ ("Unimplemented: " ++ show "MLIL_IMPORT")
    MLIL_ADD -> do
       error $ ("Unimplemented: " ++ show "MLIL_ADD")
    MLIL_ADC -> do
       error $ ("Unimplemented: " ++ show "MLIL_ADC")
    MLIL_SUB -> do
       error $ ("Unimplemented: " ++ show "MLIL_SUB")
    MLIL_SBB -> do
       error $ ("Unimplemented: " ++ show "MLIL_SBB")
    MLIL_AND -> do
       error $ ("Unimplemented: " ++ show "MLIL_AND")
    MLIL_OR -> do
       error $ ("Unimplemented: " ++ show "MLIL_OR")
    MLIL_XOR -> do
       error $ ("Unimplemented: " ++ show "MLIL_XOR")
    MLIL_LSL -> do
       error $ ("Unimplemented: " ++ show "MLIL_LSL")
    MLIL_LSR -> do
       error $ ("Unimplemented: " ++ show "MLIL_LSR")
    MLIL_ASR -> do
       error $ ("Unimplemented: " ++ show "MLIL_ASR")
    MLIL_ROL -> do
       error $ ("Unimplemented: " ++ show "MLIL_ROL")
    MLIL_RLC -> do
       error $ ("Unimplemented: " ++ show "MLIL_RLC")
    MLIL_ROR -> do
       error $ ("Unimplemented: " ++ show "MLIL_ROR")
    MLIL_RRC -> do
       error $ ("Unimplemented: " ++ show "MLIL_RRC")
    MLIL_MUL -> do
       error $ ("Unimplemented: " ++ show "MLIL_MUL")
    MLIL_MULU_DP -> do
       error $ ("Unimplemented: " ++ show "MLIL_MULU_DP")
    MLIL_MULS_DP -> do
       error $ ("Unimplemented: " ++ show "MLIL_MULS_DP")
    MLIL_DIVU -> do
       error $ ("Unimplemented: " ++ show "MLIL_DIVU")
    MLIL_DIVU_DP -> do
       error $ ("Unimplemented: " ++ show "MLIL_DIVU_DP")
    MLIL_DIVS -> do
       error $ ("Unimplemented: " ++ show "MLIL_DIVS")
    MLIL_DIVS_DP -> do
       error $ ("Unimplemented: " ++ show "MLIL_DIVS_DP")
    MLIL_MODU -> do
       error $ ("Unimplemented: " ++ show "MLIL_MODU")
    MLIL_MODU_DP -> do
       error $ ("Unimplemented: " ++ show "MLIL_MODU_DP")
    MLIL_MODS -> do
       error $ ("Unimplemented: " ++ show "MLIL_MODS")
    MLIL_MODS_DP -> do
       error $ ("Unimplemented: " ++ show "MLIL_MODS_DP")
    MLIL_NEG -> do
       error $ ("Unimplemented: " ++ show "MLIL_NEG")
    MLIL_NOT -> do
       error $ ("Unimplemented: " ++ show "MLIL_NOT")
    MLIL_SX -> do
       error $ ("Unimplemented: " ++ show "MLIL_SX")
    MLIL_ZX -> do
       error $ ("Unimplemented: " ++ show "MLIL_ZX")
    MLIL_LOW_PART -> do
       error $ ("Unimplemented: " ++ show "MLIL_LOW_PART")
    MLIL_JUMP -> do
       error $ ("Unimplemented: " ++ show "MLIL_JUMP")
    MLIL_JUMP_TO -> do
       error $ ("Unimplemented: " ++ show "MLIL_JUMP_TO")
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
      let rec = MediumLevelILRetRec { src = src' }
      return $ MediumLevelILRet rec
    MLIL_NORET -> do
       error $ ("Unimplemented: " ++ show "MLIL_NORET")
    MLIL_IF -> do
       error $ ("Unimplemented: " ++ show "MLIL_IF")
    MLIL_GOTO -> do
       error $ ("Unimplemented: " ++ show "MLIL_GOTO")
    MLIL_CMP_E -> do
       error $ ("Unimplemented: " ++ show "MLIL_CMP_E")
    MLIL_CMP_NE -> do
       error $ ("Unimplemented: " ++ show "MLIL_CMP_NE")
    MLIL_CMP_SLT -> do
       error $ ("Unimplemented: " ++ show "MLIL_CMP_SLT")
    MLIL_CMP_ULT -> do
       error $ ("Unimplemented: " ++ show "MLIL_CMP_ULT")
    MLIL_CMP_SLE -> do
       error $ ("Unimplemented: " ++ show "MLIL_CMP_SLE")
    MLIL_CMP_ULE -> do
       error $ ("Unimplemented: " ++ show "MLIL_CMP_ULE")
    MLIL_CMP_SGE -> do
       error $ ("Unimplemented: " ++ show "MLIL_CMP_SGE")
    MLIL_CMP_UGE -> do
       error $ ("Unimplemented: " ++ show "MLIL_CMP_UGE")
    MLIL_CMP_SGT -> do
       error $ ("Unimplemented: " ++ show "MLIL_CMP_SGT")
    MLIL_CMP_UGT -> do
       error $ ("Unimplemented: " ++ show "MLIL_CMP_UGT")
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
       error $ ("Unimplemented: " ++ show "MLIL_FSQRT")
    MLIL_FNEG -> do
       error $ ("Unimplemented: " ++ show "MLIL_FNEG")
    MLIL_FABS -> do
       error $ ("Unimplemented: " ++ show "MLIL_FABS")
    MLIL_FLOAT_TO_INT -> do
       error $ ("Unimplemented: " ++ show "MLIL_FLOAT_TO_INT")
    MLIL_INT_TO_FLOAT -> do
       error $ ("Unimplemented: " ++ show "MLIL_INT_TO_FLOAT")
    MLIL_FLOAT_CONV -> do
       error $ ("Unimplemented: " ++ show "MLIL_FLOAT_CONV")
    MLIL_ROUND_TO_INT -> do
       error $ ("Unimplemented: " ++ show "MLIL_ROUND_TO_INT")
    MLIL_FLOOR -> do
       error $ ("Unimplemented: " ++ show "MLIL_FLOOR")
    MLIL_CEIL -> do
       error $ ("Unimplemented: " ++ show "MLIL_CEIL")
    MLIL_FTRUNC -> do
       error $ ("Unimplemented: " ++ show "MLIL_FTRUNC")
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
       error $ ("Unimplemented: " ++ show "MLIL_SET_VAR_SSA")
    MLIL_SET_VAR_SSA_FIELD -> do
       error $ ("Unimplemented: " ++ show "MLIL_SET_VAR_SSA_FIELD")
    MLIL_SET_VAR_SPLIT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_SET_VAR_SPLIT_SSA")
    MLIL_SET_VAR_ALIASED -> do
       error $ ("Unimplemented: " ++ show "MLIL_SET_VAR_ALIASED")
    MLIL_SET_VAR_ALIASED_FIELD -> do
       error $ ("Unimplemented: " ++ show "MLIL_SET_VAR_ALIASED_FIELD")
    MLIL_VAR_SSA -> do
      src <- getSSAVar rawInst 0 1
      let rec = MediumLevelILVarSsaRec { src = src, var = src }
      return $ MediumLevelILVarSsa rec
    MLIL_VAR_SSA_FIELD -> do
       error $ ("Unimplemented: " ++ show "MLIL_VAR_SSA_FIELD")
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
      output <- case outputInst of
                MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec{ dest = d }) -> return d
                _ -> error $
                  "create: Output of MediumLevelILCallSsa: expected MediumLevelILCallOutputSsa : "
                  ++ show outputInst
      dest <- getExpr func $ getOp rawInst 1
      params <- getExprList func exprIndex' 2
      srcMem <- getInt rawInst 4
      let rec = MediumLevelILCallSsaRec
             { output = output
             , dest = dest
             , params = params
             , srcMem = srcMem
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
       error $ ("Unimplemented: " ++ show "MLIL_TAILCALL_SSA")
    MLIL_TAILCALL_UNTYPED_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_TAILCALL_UNTYPED_SSA")
    MLIL_CALL_PARAM_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_CALL_PARAM_SSA")
    MLIL_CALL_OUTPUT_SSA -> do
      destMemory <- getInt rawInst 0
      dest <- getSSAVarList func 1 2
      let rec = MediumLevelILCallOutputSsaRec
             { destMemory = destMemory
             , dest = dest
             , core = coreInst
             }
      return $ MediumLevelILCallOutputSsa rec
    MLIL_MEMORY_INTRINSIC_OUTPUT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_MEMORY_INTRINSIC_OUTPUT_SSA")
    MLIL_LOAD_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_LOAD_SSA")
    MLIL_LOAD_STRUCT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_LOAD_STRUCT_SSA")
    MLIL_STORE_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_STORE_SSA")
    MLIL_STORE_STRUCT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_STORE_STRUCT_SSA")
    MLIL_INTRINSIC_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_INTRINSIC_SSA")
    MLIL_MEMORY_INTRINSIC_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_MEMORY_INTRINSIC_SSA")
    MLIL_FREE_VAR_SLOT_SSA -> do
       error $ ("Unimplemented: " ++ show "MLIL_FREE_VAR_SLOT_SSA")
    MLIL_VAR_PHI -> do
       error $ ("Unimplemented: " ++ show "MLIL_VAR_PHI")
    MLIL_MEM_PHI -> do
       error $ ("Unimplemented: " ++ show "MLIL_MEM_PHI")
    _ -> error $ ("Unknown instruction type: " ++ show rawInst)

