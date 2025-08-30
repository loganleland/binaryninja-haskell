module ReferenceSource
  ( ReferenceSource.codeRefs
  , bnFunc
  , bnArch
  , bnAddr
  ) where

import Types


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

