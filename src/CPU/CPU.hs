module CPU.CPU where

import Control.Monad
import Control.Monad.State
import Data.Bits

import Data.Word (Word8, Word16)
import qualified Data.Vector as V

import CPU.Memory
import CPU.Data


fetch ::CPUState Word16
fetch = do
    cpu <- get
    let ptr =  pc cpu
    n1 <- readMemory ptr
    n2 <- readMemory (ptr + 1)
    -- add 2 to pc
    modify $ \cpu -> cpu {pc = ptr + 2}
    return $ combineWord8 n1 n2
    where
        combineWord8 a b = (fromIntegral a `shiftL` 8) .|. fromIntegral b



test :: Word16
test = 0xF212


-- convert word16 to hexadecimal characters
splitWord16 :: (Num a, Bits a) => a -> (a, a, a, a)
splitWord16 w = (getByte 3, getByte 2, getByte 1, getByte 0)
    where getByte n = w `shiftR` (4 * n) .&. 0xF






-- decode::  Word16 -> ( Commands
-- decode word = case getFirstNibble word of
--     (0x0, 0x0, 0xE, 0x0) -> CLEAR -- 0x00E0
--      -> JUMP word




