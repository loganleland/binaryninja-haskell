module ReferenceSource
  ( ReferenceSource.codeRefs
  , BNReferenceSource
  , bnFunc
  , bnArch
  , bnAddr
  ) where

import Types
import Foreign (alloca, peek, peekElemOff, castPtr,
                Storable (peekByteOff, pokeByteOff, poke, peek, sizeOf, alignment))

instance Storable BNReferenceSource where
  sizeOf _    = 24
  alignment _ = Types.alignmentS
  peek ptr = do
    f    <- peekByteOff ptr 0  :: IO BNFunctionPtr
    a    <- peekByteOff ptr 8  :: IO BNArchPtr
    addr <- peekByteOff ptr 16 :: IO Word64
    return $ BNReferenceSource f a addr
  poke ptr (BNReferenceSource f a addr) = do
    pokeByteOff ptr 0  f
    pokeByteOff ptr 8  a
    pokeByteOff ptr 16 addr


data BNReferenceSource = BNReferenceSource
  { bnFunc :: !BNFunctionPtr
  , bnArch :: !BNArchPtr
  , bnAddr :: !Word64
  } deriving (Eq, Show)


data CodeReferenceList = CodeReferenceList
  { crArrayPtr :: !(ForeignPtr BNFunctionPtr)
  , crCount    :: !Int
  , crList     :: ![BNReferenceSource]
  , crViewPtr  :: !BNBinaryViewPtr
  }
  deriving (Eq, Show)


foreign import ccall unsafe "BNGetCodeReferences"
  c_BNGetCodeReferences
    :: BNBinaryViewPtr
    -> Word64
    -> Ptr CSize
    -> CBool
    -> CSize
    -> IO BNReferenceSourcePtr


foreign import ccall unsafe "BNFreeCodeReferences"
  c_BNFreeCodeReferences :: BNReferenceSourcePtr -> CSize -> IO ()


-- binary view -> address -> code references
codeRefs :: BNBinaryViewPtr -> Word64 -> IO [BNReferenceSource]
codeRefs view addr =
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetCodeReferences view addr countPtr (CBool 0) 0
    count  <- fromIntegral <$> peek countPtr
    if arrPtr == nullPtr || count ==0
      then return []
      else do
        headPtr <- peekArray count (castPtr arrPtr :: Ptr BNReferenceSource)
        c_BNFreeCodeReferences arrPtr (fromIntegral count)
        return headPtr

