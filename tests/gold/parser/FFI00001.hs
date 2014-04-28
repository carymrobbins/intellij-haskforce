module FFI00001 where

foreign import ccall unsafe "wool-common.h wool_init" initWool :: CInt -> Ptr CString -> IO CInt