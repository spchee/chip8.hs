module CPU.Utilities where 
    
import Data.Word (Word8, Word16)
import Data.Bits ( Bits((.&.), shiftR) )

splitw16intoW8 ::Word16 -> (Word8, Word8)
splitw16intoW8 word = (w1, w2)
    where
        w1 = fromIntegral $ (word `shiftR` 8) .&. 0xFF
        w2 = fromIntegral $ word .&. 0xFF

-- convert word16 to hexadecimal characters
splitW16intoW4 :: Word16 -> (Word8, Word8, Word8, Word8)
splitW16intoW4 w = (getByte 3, getByte 2, getByte 1, getByte 0)
    where getByte n = fromIntegral $ w `shiftR` (4 * n) .&. 0xF

