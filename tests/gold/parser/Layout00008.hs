module Layout00008 where

-- | Calculate the normal form CRC using a table
crcNormal :: (Bits a)
          => Pull1 a -> Data a -> Pull1 Word8 -> Data a
crcNormal table initial xs = fromZero $ fold step initial xs
  where
    sz         = bitSize initial
    step crc a = (table ! (Z :. i2n ((i2n (crc .>>. (sz - 8)) .&. 0xFF) `xor` a))) `xor` (crc .<<. 8)
