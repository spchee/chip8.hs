module CPU.CPU where

import Control.Monad
import Control.Monad.State
import Data.Bits

import Data.Word (Word8, Word16)
import qualified Data.Vector as V

import CPU.Memory
import CPU.Data
import CPU.Utilities
import CPU.Commands
import Display.Display (PixelGrid)



fetch ::State CPU Word16
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

execute :: PixelGrid -> CPU -> Word16 -> (PixelGrid, CPU)
execute grid cpu w = case splitW16intoW4 w of
    (0x0, 0x0, 0xE, 0x0) -> (clear, cpu) 

    (0x1, _, _, _) -> -- Jump
        (grid, execState (jump $ w .&. 0x0FFF) cpu)

    (0x6, x, _, _) -> 
        (grid, execState (setReg x (int w .&. 0xFF)) cpu)

    (0x7, x, _, _ ) ->
        (grid, execState (addToReg x (fromIntegral w .&. 0xFF)) cpu)

    (0xA, _, _, _) ->
        (grid, execState (setIR (int w .&. 0xFFF)) cpu)

    (_, _, _, _) -> 
        (grid, cpu)

    where int a = fromIntegral a

    











-- decode::  Word16 -> ( Commands
-- decode word = case getFirstNibble word of
--     (0x0, 0x0, 0xE, 0x0) -> CLEAR -- 0x00E0
--      -> JUMP word




