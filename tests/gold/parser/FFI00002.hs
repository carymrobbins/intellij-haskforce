{-# LANGUAGE ForeignFunctionInterface, InterruptibleFFI, CApiFFI #-}
module FFI00002 where

foreign import ccall interruptible
   "sleep" sleep :: CUint -> IO CUint

foreign import ccall safe "wool-common.h wool_init" initWool :: CInt -> Ptr CString -> IO CInt
foreign import jvm interruptible "wool-common.h wool_init2" initWool2 :: CInt -> Ptr CString -> IO CInt