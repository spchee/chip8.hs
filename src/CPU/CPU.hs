module CPU.CPU where

import Control.Monad
import Control.Monad.State
import Data.Bits

import Data.Word (Word8, Word16)
import qualified Data.Vector as V

import CPU.Memory
import CPU.Data
import CPU.Commands
import Display.Display (PixelGrid)

{-- initialises the CPU
-- returns the initial state of the CPU
--}
initCPU :: CPU
initCPU = CPU {
    pc = 0x200, -- Instructions start at 0x200 in memory.
    ir = 0x0,
    memory = V.replicate 4096 0x0, --Memory is 4096 bytes long
    stack = V.replicate 16 0x0, -- Stack is 16 bytes long
    regs = V.replicate 16 0x0, -- Registers are 16 bytes long
    delay_timer = 0x0,
    sound_timer = 0x0
}



{-- fetch cpu 
-- This fetches the next instruction from memory, 
-- and increments the PC which points towards where to fetch the next instruction.
--}

fetch :: CPU -> (Word16, CPU )
fetch = runState fetch'

fetch' ::State CPU Word16
fetch' = do
    cpu <- get
    let ptr =  pc cpu
    modify $ \cpu -> cpu {pc = ptr + 2}
    return $ combineWord8 (readMemory ptr cpu) (readMemory (ptr + 1) cpu) 
    where
        combineWord8 a b = (fromIntegral a `shiftL` 8) .|. fromIntegral b -- combines two bytes together into a single opcode 


{-- This uses pattern matching with case of the decode the opcode, and then
    calls efficnetly calls the correct function to be executed whilst then returning the new PixelGrid State and CPU State. 

    This also allows me to easily extract specific nibbles from the opcode if I need them. However I still resorted to using .&.
    in many other places where I needed more than just a single nibble. 

    There is unfortunately no real better way to really do this other than a giant chunk of code. As there are so many instructions, that 
    any other potential method which simply be either more complicated or too confusing. 
--}
execute :: PixelGrid ->  Word16 -> CPU -> (PixelGrid, CPU)
execute grid  w cpu =
    case splitW16intoW4 w of
        (0x0, 0x0, 0xE, 0x0) ->  (clear, cpu)

        -- 1NNN set the PC to NNN (jump)
        (0x1, _, _, _) ->
            execState' jump 0x0FFF


        -- 2NNN call subroutine at NNN
        (0x2, _, _, _) ->
            execState' callSubroutine 0x0FFF

        -- 00EE return from subroutine from stack
        (0x0, 0x0, 0xE, 0xE) ->
            execState'' returnSubroutine 


        -- 3XNN increment pc by 2 if Vx = NN (skip) 
        (0x3, x, _ , _) ->
            execState' (skipIfEqual x) 0xFF


        -- 4XNN increment pc by 2 if Vx != NN (skip)
        (0x4, x, _, _) ->
            execState' (skipIfNotEqual x) 0xFF

        -- 5XY0 increment pc by 2 if Vx = Vy (skip)
        (0x5, x, y, 0x0) ->
            execState'' $ skipIfRegsEqual x y


        -- 9XY0 increment pc by 2 if Vx != Vy (skip)
        (0x9, x, y, 0x0) ->
            execState'' $skipIfRegsNotEqual x y

        -- 6XNN set Vx = NN
        ( 0x6, x, _, _) ->
            execState'(setReg x) 0x00FF



        -- 7XNN set Vx = Vx + NN
        (0x7, x, _, _ ) ->
            execState' (addToReg x) 0x00FF


        -- 8XY0 set Vx = Vy
        (0x8, x, y, 0x0) -> let y' = getRegVal cpu y
            in execState'' $ setReg x y'

        -- 8XY1 set Vx = Vx OR Vy
        (0x8, x, y, 0x1) ->
            execState'' $ setRegLogicOp (.|.) x y


        -- 8XY2 set Vx = Vx AND Vy
        (0x8, x, y, 0x2) ->
            execState'' $  setRegLogicOp (.&.) x y

        -- 8XY2 set Vx = Vx XOR Vy
        (0x8, x, y, 0x3) ->
            execState'' $ setRegLogicOp xor x y

        -- 8XY4 set Vx = Vx + Vy
        (0x8, x, y, 0x4) ->
            execState'' $ setRegAdd x y 

        -- 8XY5 set Vx = Vx - Vy
        (0x8, x, y, 0x5) ->
            execState'' $ setRegMinus x y

        -- 8XY4 set Vx = Vx - Vy
        (0x8, x, y, 0x7) ->
            execState'' $ setRegMinusOpposite x y

        -- 8XY6 shift Vx right by 1
        (0x8, x, _, 0x6) ->
            execState'' $ shiftRight1 x

        -- 8XYE shift Vx left by 1
        (0x8, x, _, 0xE) ->
            execState'' $ shiftLeft1 x


        -- ANNN set index register to NNN
        (0xA, _, _, _) ->
            execState' setIR 0x0FFF

        -- BNNN jump offset to NNN + V0
        (0xB, _, _, _) ->
            execState' jumpOffset 0x0FFF

        -- --CXNN Generates a random number, ANDs it with NN and stores the result in Vx
        -- (0xC, x, _, _) ->
        --     (grid, execState (random x (fromIntegral (w .&. 0xFF))) cpu)

        -- --FX1E Add VX to index register
        -- (0xF, x, 0x1, 0xE) -> 
        --     (grid, execState (addToIR x) cpu)

        --DXYN Draw sprite at Vx Vy (See Display.Display for full detailed explanation)
        (0xD, x, y, n) ->
            display cpu grid x y n

        (_, _, _, _) ->
            (grid, cpu)

        where
            int a = fromIntegral a

            -- simplifies some of the code since a lot of the functions use this same format
            execState' f b = (grid, execState ( f (int w .&. b)) cpu)
            execState'' f = (grid, execState f cpu)

            -- This splits the opcode into 4 nibbles, which makes it much easier to pattern match. 
            -- Shifting the bits made it extremely easy to extract each nibble and is also very efficient.
            splitW16intoW4 w = (getByte 3, getByte 2, getByte 1, getByte 0)
                where getByte n = fromIntegral $ w `shiftR` (4 * n) .&. 0xF













