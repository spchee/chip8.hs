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
import Debug.Trace (traceM, trace)

initCPU :: CPU
initCPU = CPU {
    pc = 0x0,
    ir = 0x0,
    memory = V.replicate 4096 0x0,
    stack = V.replicate 16 0x0,
    regs = V.replicate 16 0x0,
    delay_timer = 0x0,
    sound_timer = 0x0
}


-- fetch :: CPU -> Word16
-- fetch cpu = combineWord8 (readMemory pc' cpu) (readMemory pc' cpu +1)
--     where 
--         pc' = pc cpu
--         combineWord8 a b = (fromIntegral a `shiftL` 8) .|. fromIntegral b

fetch :: CPU -> (Word16, CPU )
fetch = trace "fetch1:" $ runState fetch'



fetch' ::State CPU Word16
fetch' = do
    traceM " fetch2"
    cpu <- get
    let ptr =  pc cpu
    modify $ \cpu -> cpu {pc = ptr + 2}
    return $ combineWord8 (readMemory ptr cpu) (readMemory (ptr + 1) cpu)
    where
        combineWord8 a b = (fromIntegral a `shiftL` 8) .|. fromIntegral b



execute :: PixelGrid ->  Word16 -> CPU -> (PixelGrid, CPU)
execute grid  w cpu = trace "execute " $ 
    case splitW16intoW4 w of
        (0x0, 0x0, 0xE, 0x0) -> trace "clear" (clear, cpu)

        (0x1, _, _, _) -> -- Jump
            trace "jump" $ execState' jump 0xFFF
            --(grid, execState (jump $ w .&. 0x0FFF) cpu)

        (0x6, x, _, _) ->
            trace "setReg" $ execState'(setReg x) 0xFF
            --(grid, execState (setReg x (int w .&. 0xFF)) cpu)

        (0x7, x, _, _ ) ->
            trace "addtoReg" $ execState' (addToReg x) 0xFF
            --(grid, execState (addToReg x (int w .&. 0xFF)) cpu)

        (0xA, _, _, _) ->
            trace "setIR" $ execState' setIR 0xFFF

        (0xD, x, y, n) ->
            trace "Display" $ display cpu grid x y n

        (_, _, _, _) ->
            trace "null"  (grid, cpu)

        where
            int a = fromIntegral a
            execState' f b = (grid, execState ( f (int w .&. b)) cpu)














-- decode::  Word16 -> ( Commands
-- decode word = case getFirstNibble word of
--     (0x0, 0x0, 0xE, 0x0) -> CLEAR -- 0x00E0
--      -> JUMP word




