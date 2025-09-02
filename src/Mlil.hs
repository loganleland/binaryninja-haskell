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
          _ -> error $ "getVar: " ++ show index ++ " not in [0, .., 4]"


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
                             _ -> error $ "getSSAVar: " ++ show indexVersion ++
                              " not in [0, .., 4]"


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


data MediumLevelILSSAInstruction =
   MediumLevelILCallSsa MediumLevelILCallSsaRec
 | MediumLevelILCallOutputSsa MediumLevelILCallOutputSsaRec
 | MediumLevelILConstPtr MediumLevelILConstPtrRec
 | MediumLevelILRet MediumLevelILRetRec
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

    MLIL_CALL_OUTPUT_SSA -> do
      destMemory <- getInt rawInst 0
      dest <- getSSAVarList func 1 2
      let rec = MediumLevelILCallOutputSsaRec
             { destMemory = destMemory
             , dest = dest
             , core = coreInst
             }
      return $ MediumLevelILCallOutputSsa rec

    MLIL_CONST_PTR -> do
      let rec = MediumLevelILConstPtrRec { constant = fromIntegral $ getOp rawInst 0 }
      return $ MediumLevelILConstPtr rec

    MLIL_RET -> do
      src <- getExprList func exprIndex' 0
      let rec = MediumLevelILRetRec { src = src }
      return $ MediumLevelILRet rec

    _ -> error $ ("Unimplemented: " ++ show coreInst)

