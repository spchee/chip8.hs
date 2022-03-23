module CPU.Utilities where
    
import Data.Word (Word8, Word16)
import Data.Bits ( Bits((.&.), shiftR) )

getFirstW8 :: Word16 -> Word8
getFirstW8 word = fromIntegral $ (word `shiftR` 8) .&. 0xFF

getSecondW8 :: Word16 -> Word8 
getSecondW8 word = fromIntegral $ word .&. 0xFF

splitw16intoW8 ::Word16 -> (Word8, Word8)
splitw16intoW8 w = (getFirstW8 w, getSecondW8 w)

-- convert word16 to hexadecimal characters
splitW16intoW4 :: Word16 -> (Word8, Word8, Word8, Word8)
splitW16intoW4 w = (getByte 3, getByte 2, getByte 1, getByte 0)
    where getByte n = fromIntegral $ w `shiftR` (4 * n) .&. 0xF

