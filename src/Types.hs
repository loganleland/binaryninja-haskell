module Types
  ( CSize(..)
  , CBool(..)
  , CInt(..)
  , CUInt(..)
  , CULLong(..)
  , Word8
  , Word32
  , Word64
  , Int64
  , CChar
  , Ptr
  , FunPtr
  , nullFunPtr
  , nullPtr
  , CString
  , withCString
  , newCString
  , peekCString
  , GHC.ForeignPtr.ForeignPtr
  , (.&.)
  , castWord32ToFloat 
  , castWord64ToDouble
  , float2Double 
  , newForeignPtr 
  , pointerSize
  , peek
  , peekElemOff
  , alloca
  , castPtr
  , poke
  , peekArray
  , forM
  , when
  , BNBinaryView
  , BNBinaryViewPtr
  , BNProgressFunction
  , BNProgressFunctionPtr
  , BNFunctionPtr
  , BNSymbolPtr
  , BNNameSpace
  , BNNameSpacePtr
  , BNStringRef(..)
  , BNStringRefPtr
  , BNStringType(..)
  , BNDataBufferPtr
  , BNReferenceSourcePtr
  , BNArchPtr
  , BNMlilFunctionPtr
  , BNLlilFunctionPtr
  , BNLowLevelILInstruction(..)
  , BNLowLevelILOperation(..)
  , BNMediumLevelILInstruction(..)
  , BNMediumLevelILOperation(..)
  , BNVariable(..)
  , BNSSAVariable(..)
  , TargetMap
  , Function(..)
  , FunctionList(..)
  , SymbolList(..)
  , SymbolType(..)
  , SymbolBinding(..)
  , BNRegisterValueType(..)
  , Types.alignment
  ) where


import Foreign (alloca, peek, peekElemOff, castPtr,
                Storable (peekByteOff, pokeByteOff, poke, peek, sizeOf, alignment))
import Data.Word (Word8, Word32, Word64)
import Data.Bits ((.&.))
import Data.Int (Int64)
import Foreign.Ptr (Ptr, nullFunPtr, nullPtr, FunPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.C.String (CString, withCString, newCString, peekCString)
import Foreign.C.Types (CSize(..), CBool(..), CChar, CInt(..), CUInt(..), CULLong(..))
import Foreign.Marshal.Array (peekArray)
import GHC.ForeignPtr (ForeignPtr)
import GHC.Float (float2Double, castWord32ToFloat, castWord64ToDouble, float2Double)
import Control.Monad (forM, when)


pointerSize :: Int
pointerSize = sizeOf (undefined :: Ptr ())


alignment :: Int
alignment = 8


-------------------------
-- BinaryView Types
-------------------------

data BNBinaryView
type BNBinaryViewPtr = Ptr BNBinaryView
-- | Type for the BNProgressFunction callback:
--   It is given a context pointer and two CSize values (e.g., current and total progress)
type BNProgressFunction = Ptr () -> CSize -> CSize -> IO CBool
type BNProgressFunctionPtr = FunPtr BNProgressFunction
data BNNameSpace
type BNNameSpacePtr = Ptr BNNameSpace
data BNFunction_
type BNFunctionPtr = Ptr BNFunction_
data BNSymbol_
type BNSymbolPtr = Ptr BNSymbol_
type BNStringRefPtr = Ptr BNStringRef
data BNDataBuffer_
type BNDataBufferPtr = Ptr BNDataBuffer_
data BNReferenceSource_
type BNReferenceSourcePtr = Ptr BNReferenceSource_
data BNArch_
type BNArchPtr = Ptr BNArch_
data BNMlilFunction_
type BNMlilFunctionPtr = Ptr BNMlilFunction_
data BNLlilFunction_
type BNLlilFunctionPtr = Ptr BNLlilFunction_

type TargetMap = [(CULLong, CULLong)]

data Function = Function
  { funcAdvancedAnalysisRequests :: !Int
  , funcPtr :: !BNFunctionPtr
  , viewPtr :: !BNBinaryViewPtr
  --, arch :: !BNArch
  --, platform :: !BNPlatform
  }
  deriving (Eq, Show) 


data BNStringRef = BNStringRef
  { bnType   :: !BNStringType
  , bnStart  :: !Word64
  , bnLength :: !CSize
  } deriving (Eq, Show)


instance Storable BNStringRef where
  sizeOf _ = 24
  alignment _ = Types.alignment
  peek ptr = do
    t  <- toEnum . fromIntegral <$> (peekByteOff ptr 0 :: IO CInt)
    s  <- peekByteOff ptr 8  :: IO Word64
    l  <- peekByteOff ptr 16 :: IO CSize
    return (BNStringRef t s l)
  poke ptr (BNStringRef t s l) = do
    pokeByteOff ptr 0 $ fromEnum t
    pokeByteOff ptr 8 s
    pokeByteOff ptr 16 l


data BNVariable = BNVariable
  { varSourceType   :: !Word64
  , varRef  :: !CSize
  , varStorage :: !CInt
  } deriving (Eq, Show)


instance Storable BNVariable where
  sizeOf _ = 24
  alignment _ = Types.alignment
  peek ptr = do
    t  <- peekByteOff ptr 0 :: IO Word64
    r  <- peekByteOff ptr 8  :: IO CSize
    s  <- peekByteOff ptr 16 :: IO CInt
    return (BNVariable t r s)
  poke ptr (BNVariable t r s) = do
    pokeByteOff ptr 0 t
    pokeByteOff ptr 8 r
    pokeByteOff ptr 16 s


data BNSSAVariable = BNSSAVariable
  { var :: BNVariable
  , version :: Int
  } deriving (Eq, Show)


data FunctionList = FunctionList
  { flArrayPtr :: !(ForeignPtr BNFunctionPtr)
  , flCount    :: !Int
  , flList     :: ![BNFunctionPtr]
  , flViewPtr  :: !BNBinaryViewPtr
  }
  deriving (Eq, Show)


data SymbolList = SymbolList
  { slArrayPtr :: !(ForeignPtr BNSymbolPtr)
  , slCount    :: !Int
  , slList     :: ![BNSymbolPtr]
  , slViewPtr  :: !BNBinaryViewPtr
  }
  deriving (Eq, Show)


data SymbolType = FunctionSymbol | ImportAddressSymbol | ImportedFunctionSymbol |
                  DataSymbol | ImportedDataSymbol | ExternalSymbol |
                  LibraryFunctionSymbol | SymbolicFunctionSymbol | LocalLabelSymbol
  deriving (Eq, Show, Enum)


data SymbolBinding = NoBinding | LocalBinding | GlobalBinding | WeakBinding
  deriving (Eq, Show, Enum)


data BNStringType = AsciiString | Utf16String | Utf32String | Utf8String
  deriving (Eq, Show, Enum)


data BNLowLevelILInstruction = BNLowLevelILInstruction
  { llOperation     :: !Word32
  , llAttributes    :: !Word32
  , llSize          :: !CSize
  , llFlags         :: !CUInt
  , llSourceOperand :: !CUInt
  , llOp0           :: !CULLong
  , llOp1           :: !CULLong
  , llOp2           :: !CULLong
  , llOp3           :: !CULLong
  , llAddress       :: !CULLong
  } deriving (Eq, Show)


instance Storable BNLowLevelILInstruction where
  sizeOf _ = 64
  alignment _ = Types.alignment 
  peek ptr = do
    op <- peekByteOff ptr 0
    attr <- peekByteOff ptr 4
    sz <- peekByteOff ptr 8
    flg <- peekByteOff ptr 16
    srcOp <- peekByteOff ptr 20
    o0 <- peekByteOff ptr 24
    o1 <- peekByteOff ptr 32
    o2 <- peekByteOff ptr 40
    o3 <- peekByteOff ptr 48
    addr <- peekByteOff ptr 56
    return (BNLowLevelILInstruction op attr sz flg srcOp o0 o1 o2 o3 addr)
  poke ptr (BNLowLevelILInstruction op attr sz flg srcOp o0 o1 o2 o3 addr) = do
    pokeByteOff ptr 0 op
    pokeByteOff ptr 4 attr
    pokeByteOff ptr 8 sz
    pokeByteOff ptr 16 flg
    pokeByteOff ptr 20 srcOp
    pokeByteOff ptr 24 o0
    pokeByteOff ptr 32 o1
    pokeByteOff ptr 40 o2
    pokeByteOff ptr 48 o3
    pokeByteOff ptr 56 addr


data BNLowLevelILOperation = LLIL_NOP | LLIL_SET_REG | LLIL_SET_REG_SPLIT | LLIL_SET_FLAG | LLIL_SET_REG_STACK_REL | LLIL_REG_STACK_PUSH | LLIL_ASSERT | LLIL_FORCE_VER | LLIL_LOAD | LLIL_STORE | LLIL_PUSH | LLIL_POP | LLIL_REG | LLIL_REG_SPLIT | LLIL_REG_STACK_REL | LLIL_REG_STACK_POP | LLIL_REG_STACK_FREE_REG | LLIL_REG_STACK_FREE_REL | LLIL_CONST | LLIL_CONST_PTR | LLIL_EXTERN_PTR | LLIL_FLOAT_CONST | LLIL_FLAG | LLIL_FLAG_BIT | LLIL_ADD | LLIL_ADC | LLIL_SUB | LLIL_SBB | LLIL_AND | LLIL_OR | LLIL_XOR | LLIL_LSL | LLIL_LSR | LLIL_ASR | LLIL_ROL | LLIL_RLC | LLIL_ROR | LLIL_RRC | LLIL_MUL | LLIL_MULU_DP | LLIL_MULS_DP | LLIL_DIVU | LLIL_DIVU_DP | LLIL_DIVS | LLIL_DIVS_DP | LLIL_MODU | LLIL_MODU_DP | LLIL_MODS | LLIL_MODS_DP | LLIL_NEG | LLIL_NOT | LLIL_SX | LLIL_ZX | LLIL_LOW_PART | LLIL_JUMP | LLIL_JUMP_TO | LLIL_CALL | LLIL_CALL_STACK_ADJUST | LLIL_TAILCALL | LLIL_RET | LLIL_NORET | LLIL_IF | LLIL_GOTO | LLIL_FLAG_COND | LLIL_FLAG_GROUP | LLIL_CMP_E | LLIL_CMP_NE | LLIL_CMP_SLT | LLIL_CMP_ULT | LLIL_CMP_SLE | LLIL_CMP_ULE | LLIL_CMP_SGE | LLIL_CMP_UGE | LLIL_CMP_SGT | LLIL_CMP_UGT | LLIL_TEST_BIT | LLIL_BOOL_TO_INT | LLIL_ADD_OVERFLOW | LLIL_SYSCALL | LLIL_BP | LLIL_TRAP | LLIL_INTRINSIC | LLIL_UNDEF | LLIL_UNIMPL | LLIL_UNIMPL_MEM | LLIL_FADD | LLIL_FSUB | LLIL_FMUL | LLIL_FDIV | LLIL_FSQRT | LLIL_FNEG | LLIL_FABS | LLIL_FLOAT_TO_INT | LLIL_INT_TO_FLOAT | LLIL_FLOAT_CONV | LLIL_ROUND_TO_INT | LLIL_FLOOR | LLIL_CEIL | LLIL_FTRUNC | LLIL_FCMP_E | LLIL_FCMP_NE | LLIL_FCMP_LT | LLIL_FCMP_LE | LLIL_FCMP_GE | LLIL_FCMP_GT | LLIL_FCMP_O | LLIL_FCMP_UO | LLIL_SET_REG_SSA | LLIL_SET_REG_SSA_PARTIAL | LLIL_SET_REG_SPLIT_SSA | LLIL_SET_REG_STACK_REL_SSA | LLIL_SET_REG_STACK_ABS_SSA | LLIL_REG_SPLIT_DEST_SSA | LLIL_REG_STACK_DEST_SSA | LLIL_REG_SSA | LLIL_REG_SSA_PARTIAL | LLIL_REG_SPLIT_SSA | LLIL_REG_STACK_REL_SSA | LLIL_REG_STACK_ABS_SSA | LLIL_REG_STACK_FREE_REL_SSA | LLIL_REG_STACK_FREE_ABS_SSA | LLIL_SET_FLAG_SSA | LLIL_ASSERT_SSA | LLIL_FORCE_VER_SSA | LLIL_FLAG_SSA | LLIL_FLAG_BIT_SSA | LLIL_CALL_SSA | LLIL_SYSCALL_SSA | LLIL_TAILCALL_SSA | LLIL_CALL_PARAM | LLIL_CALL_STACK_SSA | LLIL_CALL_OUTPUT_SSA | LLIL_SEPARATE_PARAM_LIST_SSA | LLIL_SHARED_PARAM_SLOT_SSA | LLIL_MEMORY_INTRINSIC_OUTPUT_SSA | LLIL_LOAD_SSA | LLIL_STORE_SSA | LLIL_INTRINSIC_SSA | LLIL_MEMORY_INTRINSIC_SSA | LLIL_REG_PHI | LLIL_REG_STACK_PHI | LLIL_FLAG_PHI | LLIL_MEM_PHI
  deriving (Eq, Show, Enum)


data BNMediumLevelILOperation =  MLIL_NOP | MLIL_SET_VAR | MLIL_SET_VAR_FIELD | MLIL_SET_VAR_SPLIT | MLIL_ASSERT | MLIL_FORCE_VER | MLIL_LOAD | MLIL_LOAD_STRUCT | MLIL_STORE | MLIL_STORE_STRUCT | MLIL_VAR | MLIL_VAR_FIELD | MLIL_VAR_SPLIT | MLIL_ADDRESS_OF | MLIL_ADDRESS_OF_FIELD | MLIL_CONST | MLIL_CONST_DATA | MLIL_CONST_PTR | MLIL_EXTERN_PTR | MLIL_FLOAT_CONST | MLIL_IMPORT | MLIL_ADD | MLIL_ADC | MLIL_SUB | MLIL_SBB | MLIL_AND | MLIL_OR | MLIL_XOR | MLIL_LSL | MLIL_LSR | MLIL_ASR | MLIL_ROL | MLIL_RLC | MLIL_ROR | MLIL_RRC | MLIL_MUL | MLIL_MULU_DP | MLIL_MULS_DP | MLIL_DIVU | MLIL_DIVU_DP | MLIL_DIVS | MLIL_DIVS_DP | MLIL_MODU | MLIL_MODU_DP | MLIL_MODS | MLIL_MODS_DP | MLIL_NEG | MLIL_NOT | MLIL_SX | MLIL_ZX | MLIL_LOW_PART | MLIL_JUMP | MLIL_JUMP_TO | MLIL_RET_HINT | MLIL_CALL | MLIL_CALL_UNTYPED | MLIL_CALL_OUTPUT | MLIL_CALL_PARAM | MLIL_SEPARATE_PARAM_LIST | MLIL_SHARED_PARAM_SLOT | MLIL_RET | MLIL_NORET | MLIL_IF | MLIL_GOTO | MLIL_CMP_E | MLIL_CMP_NE | MLIL_CMP_SLT | MLIL_CMP_ULT | MLIL_CMP_SLE | MLIL_CMP_ULE | MLIL_CMP_SGE | MLIL_CMP_UGE | MLIL_CMP_SGT | MLIL_CMP_UGT | MLIL_TEST_BIT | MLIL_BOOL_TO_INT | MLIL_ADD_OVERFLOW | MLIL_SYSCALL | MLIL_SYSCALL_UNTYPED | MLIL_TAILCALL | MLIL_TAILCALL_UNTYPED | MLIL_INTRINSIC | MLIL_FREE_VAR_SLOT | MLIL_BP | MLIL_TRAP | MLIL_UNDEF | MLIL_UNIMPL | MLIL_UNIMPL_MEM | MLIL_FADD | MLIL_FSUB | MLIL_FMUL | MLIL_FDIV | MLIL_FSQRT | MLIL_FNEG | MLIL_FABS | MLIL_FLOAT_TO_INT | MLIL_INT_TO_FLOAT | MLIL_FLOAT_CONV | MLIL_ROUND_TO_INT | MLIL_FLOOR | MLIL_CEIL | MLIL_FTRUNC | MLIL_FCMP_E | MLIL_FCMP_NE | MLIL_FCMP_LT | MLIL_FCMP_LE | MLIL_FCMP_GE | MLIL_FCMP_GT | MLIL_FCMP_O | MLIL_FCMP_UO | MLIL_SET_VAR_SSA | MLIL_SET_VAR_SSA_FIELD | MLIL_SET_VAR_SPLIT_SSA | MLIL_SET_VAR_ALIASED | MLIL_SET_VAR_ALIASED_FIELD | MLIL_VAR_SSA | MLIL_VAR_SSA_FIELD | MLIL_VAR_ALIASED | MLIL_VAR_ALIASED_FIELD | MLIL_VAR_SPLIT_SSA | MLIL_ASSERT_SSA | MLIL_FORCE_VER_SSA | MLIL_CALL_SSA | MLIL_CALL_UNTYPED_SSA | MLIL_SYSCALL_SSA | MLIL_SYSCALL_UNTYPED_SSA | MLIL_TAILCALL_SSA | MLIL_TAILCALL_UNTYPED_SSA | MLIL_CALL_PARAM_SSA | MLIL_CALL_OUTPUT_SSA | MLIL_MEMORY_INTRINSIC_OUTPUT_SSA | MLIL_LOAD_SSA | MLIL_LOAD_STRUCT_SSA | MLIL_STORE_SSA | MLIL_STORE_STRUCT_SSA | MLIL_INTRINSIC_SSA | MLIL_MEMORY_INTRINSIC_SSA | MLIL_FREE_VAR_SLOT_SSA | MLIL_VAR_PHI | MLIL_MEM_PHI
  deriving (Eq, Show, Enum)


data BNMediumLevelILInstruction = BNMediumLevelILInstruction
  { mlOperation     :: !Word32
  , mlAttributes    :: !Word32
  , mlSourceOperand :: !CUInt
  , mlSize          :: !CSize
  , mlOp0           :: !CULLong
  , mlOp1           :: !CULLong
  , mlOp2           :: !CULLong
  , mlOp3           :: !CULLong
  , mlOp4           :: !CULLong
  , mlAddress       :: !CULLong
  } deriving (Eq, Show)


instance Storable BNMediumLevelILInstruction where
  sizeOf _ = 72
  alignment _ = Types.alignment 
  peek ptr = do
    op <- peekByteOff ptr 0
    attr <- peekByteOff ptr 4
    srcOp <- peekByteOff ptr 8
    sz <- peekByteOff ptr 16
    o0 <- peekByteOff ptr 24
    o1 <- peekByteOff ptr 32
    o2 <- peekByteOff ptr 40
    o3 <- peekByteOff ptr 48
    o4 <- peekByteOff ptr 56
    addr <- peekByteOff ptr 64
    return (BNMediumLevelILInstruction op attr srcOp sz o0 o1 o2 o3 o4 addr)
  poke ptr (BNMediumLevelILInstruction op attr srcOp sz o0 o1 o2 o3 o4 addr) = do
    pokeByteOff ptr 0 op
    pokeByteOff ptr 4 attr
    pokeByteOff ptr 8 srcOp
    pokeByteOff ptr 16 sz
    pokeByteOff ptr 24 o0
    pokeByteOff ptr 32 o1
    pokeByteOff ptr 40 o2
    pokeByteOff ptr 48 o3
    pokeByteOff ptr 56 o4
    pokeByteOff ptr 64 addr



data BNRegisterValueType
  = UndeterminedValue
  | EntryValue
  | ConstantValue
  | ConstantPointerValue
  | ExternalPointerValue
  | StackFrameOffset
  | ReturnAddressValue
  | ImportedAddressValue
  | SignedRangeValue
  | UnsignedRangeValue
  | LookupTableValue
  | InSetOfValues
  | NotInSetOfValues
  | ConstantDataValue
  | ConstantDataZeroExtendValue
  | ConstantDataSignExtendValue
  | ConstantDataAggregateValue
  deriving (Eq, Show)


instance Enum BNRegisterValueType where
  fromEnum UndeterminedValue           = 0
  fromEnum EntryValue                  = 1
  fromEnum ConstantValue               = 2
  fromEnum ConstantPointerValue        = 3
  fromEnum ExternalPointerValue        = 4
  fromEnum StackFrameOffset            = 5
  fromEnum ReturnAddressValue          = 6
  fromEnum ImportedAddressValue        = 7
  fromEnum SignedRangeValue            = 8
  fromEnum UnsignedRangeValue          = 9
  fromEnum LookupTableValue            = 10
  fromEnum InSetOfValues               = 11
  fromEnum NotInSetOfValues            = 12
  fromEnum ConstantDataValue           = 0x8000
  fromEnum ConstantDataZeroExtendValue = 0x8001
  fromEnum ConstantDataSignExtendValue = 0x8002
  fromEnum ConstantDataAggregateValue  = 0x8003

  toEnum 0x0000 = UndeterminedValue
  toEnum 0x0001 = EntryValue
  toEnum 0x0002 = ConstantValue
  toEnum 0x0003 = ConstantPointerValue
  toEnum 0x0004 = ExternalPointerValue
  toEnum 0x0005 = StackFrameOffset
  toEnum 0x0006 = ReturnAddressValue
  toEnum 0x0007 = ImportedAddressValue
  toEnum 0x0008 = SignedRangeValue
  toEnum 0x0009 = UnsignedRangeValue
  toEnum 0x000A = LookupTableValue
  toEnum 0x000B = InSetOfValues
  toEnum 0x000C = NotInSetOfValues
  toEnum 0x8000 = ConstantDataValue
  toEnum 0x8001 = ConstantDataZeroExtendValue
  toEnum 0x8002 = ConstantDataSignExtendValue
  toEnum 0x8003 = ConstantDataAggregateValue
  toEnum n      = error $ "BNRegisterValueType.toEnum: invalid tag " ++ show n

