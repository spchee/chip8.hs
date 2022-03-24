module CPU.Data where
import Data.Word (Word16, Word8)
import Data.Vector as V (Vector, (!), (//), length, (++), snoc, unsnoc)
import Control.Monad.State
import Control.Monad


{--   I used vectors to store memory and all my registers as it'd have O(1) access time. 
    And since I'd have to access memory and registers very very often,
    I wanted to avoid at all costs using lists which have O(n) access time.

    Vectors are also more efficient than lists in when it comes to concatenation, changing values,
    and appending to the end of the vector, etc. 

    I also decided to contain the PC, IR, memory, etc, all within the same data structure so that I could
    easily access and change multiple at a time if needed within the CPU state monad. Using record syntax
    also makes it very easy to access and modify. 

    I decided to use Word8/Word16 for all the registers and memory because not only would it be more accurate to
    the real machine and more space efficient, but it would also allow me to use bitwise operations more cleanly. 
=-}
data CPU = CPU {
    pc :: Word16,
    ir:: Word16,
    memory :: V.Vector Word8,
    stack :: V.Vector Word16,
    regs :: V.Vector Word8,
    delay_timer :: Word8,
    sound_timer :: Word8

} deriving (Show)

{-- decrementDelay timer
-- This will decrement the delay timer by one if it is over 0. 
--}
decrementDelayTimer ::  State CPU ()
decrementDelayTimer = do
    delay_timer <- gets delay_timer
    Control.Monad.when (delay_timer > 0) $ do
        modify $ \cpu -> cpu {delay_timer = delay_timer - 1}

{-- getPC 
-- This will returns the current state of the program counter
--}
getPC :: State CPU Word16
getPC = gets pc

{-- setPC
-- This will set the program counter to the value passed in and 
-- return the new state of the CPU with the new PC. 
--}

setPC :: Word16 -> State CPU ()
setPC pc' = modify $ \cpu -> cpu { pc = pc' }

{-- setRegVal val address
--  This will set the value of the register at the address passed in.
-- Returns the new state of the CPU with the new register value.
--}
setRegVal :: Word8 -> Word8 -> State CPU ()
setRegVal val ptr = do
    cpu <- get
    put cpu { regs = regs cpu V.// [(fromIntegral ptr, val)] }

{-- getRegVal CPU address
-- This will get the value of the register at the address passed in.
--}
getRegVal :: CPU -> Word8-> Word8
getRegVal cpu ptr = regs cpu V.! fromIntegral ptr

{-- setFlagReg val 
-- This sets the flag register to the value passed in. The flag register is used to detect collisions. 
--}
setFlagReg ::  Word8 -> State CPU()
setFlagReg  flag = do
    modify $ \cpu -> cpu { regs = regs cpu V.// [(0xF, flag)] }


{-- push stack cpu value
This pushes the value passed in onto the stack. The stack is used to store the return address of a subroutine.
I decided to make actual proper pop and push functions to decrease the likely hood that I accidently accseed a different
value I wasn't meant to. 
-}
pushStack :: CPU -> Word16 -> State CPU ()
pushStack cpu val= do
    modify $ \cpu' -> cpu' { stack = stack cpu `V.snoc` val }

{-- popStack cpu
-- This pops the top value off of the stack and returns it. If the stack is empty, it'll return 0. 
-- I used pattern matching here to extract values from the maybe and to easily check if the stack is empty.

    Returns the new state of the CPU with the new stack and the popped value (if there is one).
--}
popStack :: CPU -> State CPU Word16
popStack cpu = do
    stack' <- gets stack
    let stack'' = V.unsnoc stack' 
    case stack'' of
        Nothing -> return 0
        Just (stack'', pc') -> do
            modify $ \cpu' -> cpu' { stack = stack'' }
            return pc'

