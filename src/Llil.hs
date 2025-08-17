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


data BNLowLevelILOperation = LLIL_NOP | LLIL_SET_REG | LLIL_SET_REG_SPLIT | LLIL_SET_FLAG | LLIL_SET_REG_STACK_REL | LLIL_REG_STACK_PUSH | LLIL_ASSERT | LLIL_FORCE_VER | LLIL_LOAD | LLIL_STORE | LLIL_PUSH | LLIL_POP | LLIL_REG | LLIL_REG_SPLIT | LLIL_REG_STACK_REL | LLIL_REG_STACK_POP | LLIL_REG_STACK_FREE_REG | LLIL_REG_STACK_FREE_REL | LLIL_CONST | LLIL_CONST_PTR | LLIL_EXTERN_PTR | LLIL_FLOAT_CONST | LLIL_FLAG | LLIL_FLAG_BIT | LLIL_ADD | LLIL_ADC | LLIL_SUB | LLIL_SBB | LLIL_AND | LLIL_OR | LLIL_XOR | LLIL_LSL | LLIL_LSR | LLIL_ASR | LLIL_ROL | LLIL_RLC | LLIL_ROR | LLIL_RRC | LLIL_MUL | LLIL_MULU_DP | LLIL_MULS_DP | LLIL_DIVU | LLIL_DIVU_DP | LLIL_DIVS | LLIL_DIVS_DP | LLIL_MODU | LLIL_MODU_DP | LLIL_MODS | LLIL_MODS_DP | LLIL_NEG | LLIL_NOT | LLIL_SX | LLIL_ZX | LLIL_LOW_PART | LLIL_JUMP | LLIL_JUMP_TO | LLIL_CALL | LLIL_CALL_STACK_ADJUST | LLIL_TAILCALL | LLIL_RET | LLIL_NORET | LLIL_IF | LLIL_GOTO | LLIL_FLAG_COND | LLIL_FLAG_GROUP | LLIL_CMP_E | LLIL_CMP_NE | LLIL_CMP_SLT | LLIL_CMP_ULT | LLIL_CMP_SLE | LLIL_CMP_ULE | LLIL_CMP_SGE | LLIL_CMP_UGE | LLIL_CMP_SGT | LLIL_CMP_UGT | LLIL_TEST_BIT | LLIL_BOOL_TO_INT | LLIL_ADD_OVERFLOW | LLIL_SYSCALL | LLIL_BP | LLIL_TRAP | LLIL_INTRINSIC | LLIL_UNDEF | LLIL_UNIMPL | LLIL_UNIMPL_MEM | LLIL_FADD | LLIL_FSUB | LLIL_FMUL | LLIL_FDIV | LLIL_FSQRT | LLIL_FNEG | LLIL_FABS | LLIL_FLOAT_TO_INT | LLIL_INT_TO_FLOAT | LLIL_FLOAT_CONV | LLIL_ROUND_TO_INT | LLIL_FLOOR | LLIL_CEIL | LLIL_FTRUNC | LLIL_FCMP_E | LLIL_FCMP_NE | LLIL_FCMP_LT | LLIL_FCMP_LE | LLIL_FCMP_GE | LLIL_FCMP_GT | LLIL_FCMP_O | LLIL_FCMP_UO | LLIL_SET_REG_SSA | LLIL_SET_REG_SSA_PARTIAL | LLIL_SET_REG_SPLIT_SSA | LLIL_SET_REG_STACK_REL_SSA | LLIL_SET_REG_STACK_ABS_SSA | LLIL_REG_SPLIT_DEST_SSA | LLIL_REG_STACK_DEST_SSA | LLIL_REG_SSA | LLIL_REG_SSA_PARTIAL | LLIL_REG_SPLIT_SSA | LLIL_REG_STACK_REL_SSA | LLIL_REG_STACK_ABS_SSA | LLIL_REG_STACK_FREE_REL_SSA | LLIL_REG_STACK_FREE_ABS_SSA | LLIL_SET_FLAG_SSA | LLIL_ASSERT_SSA | LLIL_FORCE_VER_SSA | LLIL_FLAG_SSA | LLIL_FLAG_BIT_SSA | LLIL_CALL_SSA | LLIL_SYSCALL_SSA | LLIL_TAILCALL_SSA | LLIL_CALL_PARAM | LLIL_CALL_STACK_SSA | LLIL_CALL_OUTPUT_SSA | LLIL_SEPARATE_PARAM_LIST_SSA | LLIL_SHARED_PARAM_SLOT_SSA | LLIL_MEMORY_INTRINSIC_OUTPUT_SSA | LLIL_LOAD_SSA | LLIL_STORE_SSA | LLIL_INTRINSIC_SSA | LLIL_MEMORY_INTRINSIC_SSA | LLIL_REG_PHI | LLIL_REG_STACK_PHI | LLIL_FLAG_PHI | LLIL_MEM_PHI
  deriving (Eq, Show, Enum)


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
    start <- c_BNLowLevelILGetInstructionStart func arch addr        
    count <- c_BNGetLowLevelILInstructionCount func
    -- Ensure start index is less than total llil instructions
    -- in function
    if start >= count
    then return Nothing
    else return $ Just start


foreign import ccall unsafe "BNGetLowLevelILIndexForInstruction"
  c_BNGetLowLevelILIndexForInstruction
    :: BNLlilFunctionPtr -> Word64 -> IO CSize


-- Convert an instruction index into an expression index
instIndexToExprIndex :: BNLlilFunctionPtr -> Word64 -> IO CSize
instIndexToExprIndex = c_BNGetLowLevelILIndexForInstruction


foreign import ccall unsafe "BNGetLowLevelILByIndexPtr"
  c_BNGetLowLevelILByIndexPtr
    :: BNLlilFunctionPtr -> CSize -> IO (Ptr BNLowLevelILInstruction)


llilByIndex :: BNLlilFunctionPtr -> CSize -> IO (Maybe BNLowLevelILInstruction)
llilByIndex func index = do
  p <- c_BNGetLowLevelILByIndexPtr func index
  if p == nullPtr
  then return Nothing
  else Just <$> peek p


-- Retrieve the best LLIL instruction for the address in BNReferenceSource
fromRef :: BNReferenceSource -> IO (Maybe BNLowLevelILInstruction)
fromRef ref = do
  func <- llil (bnFunc ref)
  case func of
    Nothing -> return Nothing
    Just func' -> do
      sIndex <- startIndex func' (bnArch ref) (bnAddr ref)
      case sIndex of
        Nothing -> return Nothing
        Just sIndex' -> do
          exprIndex <- instIndexToExprIndex func' (fromIntegral sIndex')
          llilByIndex func' exprIndex


at :: BNBinaryViewPtr -> Word64 -> IO (Maybe BNLowLevelILInstruction)
at view addr = do
  rawFunc <- head <$> functionsContaining view addr
  llilFunc <- Function.llil rawFunc 
  case llilFunc of
    Nothing -> return Nothing
    Just func' -> do
      sIndex <- startIndex func' (Function.architecture rawFunc) addr
      case sIndex of
        Nothing -> return Nothing
        Just sIndex' -> do
          exprIndex <- instIndexToExprIndex func' (fromIntegral sIndex')
          llilByIndex func' exprIndex

