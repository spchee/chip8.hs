
module CPU.Commands where
import Display.Display

import Data.Word (Word16, Word8)
import CPU.Data ( CPU(ir, regs, pc), getPC, setPC, getRegVal, pushStack, popStack, setFlagReg )
import Data.Vector as V ( (//), (!), Vector, replicate, null, empty, filter, fromList, snoc, (++), map, update, any)
import Control.Monad.State
import Graphics.Gloss.Data.Picture
import Graphics.Gloss
import Debug.Trace
import CPU.Memory (readMemory)
import Data.Bits ((.&.), Bits (shiftR, (.|.), xor, shiftL))
import qualified Control.Monad
import System.Random



{-- clear
    This clears all 2048 pixels on the screen to a completely black screen.
--}
clear :: V.Vector Colour
clear = V.replicate 2048 Black


{-- jump address (1NNN)
    This sets the program counter to the address specified (NNN), meaning that the next instruction will be executed from that address. 

    Returns: The CPU state after the jump.
--}
jump :: Word16 -> State CPU ()
jump = setPC



{-- setReg address value (6XNN)
    This sets the value of the register specified by the address (VX) to the value specified (NN), 

    Returns: The CPU state after the registers value has been changed.
--}
setReg :: Word8 -> Word8 -> State CPU  ()
setReg ptr val = do
    cpu <- get
    put cpu { regs = regs cpu V.// [(fromIntegral ptr, val)] }


{-- addToReg addrress value (7XNN)
    This adds the value specified (NN) to the register specified by the address (VX) 

    Returns: The CPU state after the registers value has been changed.
--}
addToReg :: Word8 -> Word8 -> State CPU ()
addToReg ptr val = do
    regs' <- gets regs
    modify $ \c -> c { regs = regs' V.//  valueToChange regs'} 

    where
        valueToChange regs = [(fromIntegral ptr, (regs V.! fromIntegral ptr) + val)]


{-- setIR value (ANNN)
    This sets the value of the instruction register to the value specified (NNN). 

    Returns: The CPU state after the instruction register has been changed.

--}
setIR :: Word16 -> State CPU ()
setIR val = do
    cpu <- get
    put cpu { ir = val }


{-- Display cpu -> pixels -> x -> y -> height (DXYN)

    This will update specific pixels on the screen up to a width of 8 (to the right) and a height of n.

    There are a few main stages to the updating the display:
    1. Find the x and y coordinates which are contained within the Vx and Vy registers. 
        If they go off the screen, use modulus to wrap them around. 

    2. Get the byte sprite data which contains what pixels in each row needs to be updated. Thsi is contained within memeory 
        and we use the address in the instruction register. 

    3.  Flip each pixel's colour which has a 1 in the bit position of the byte data. (don't wrap the screen here though)

    Example:    1010 1010 and  BBBB BBBB should update to WBWB WBWB (where W is white and B is black) 
                1111 0001 and WWWB WWBB should update to BBBW WWBW

    4. Continue this for each row downwards n times until the specified height is reached (or it reaches the top). Each time you move up a row, you get 
        the next byte of sprite data from memory the next address in memory (but don't increment the instruction register)

    5. If at any point, a white pixel was turned off to black, the VF flag register should be set to 0x01. If not it should be set to 0x01. 

    I firstly created a function 'colourRow' which would return the 1d coordinate of all pixels in a single row whose colour needed to be changed.
    This allowed me to also easily remove pixels which had wrapped round or went off the screen. 

    I then was able to use recursion with the 'colourRow' function to find the 1D coordinate of every pixel for every row up to the specified height 
    which needed to be updated. I was then able to proceed to update the pixels in the grid using V.update. 

    I also had a pure-function display which I used so that I could access both the Pixel Grid State and update it, and also the CPU state
    so that I could update the flag resgister.

    I used Vector rather than pure lists as it has faster accessing times and is more efficient in general.

--}

display :: CPU -> PixelGrid -> Word8 -> Word8 -> Word8 -> (PixelGrid, CPU)
display cpu grid x y n = (grid', setReg0xF a)

    where
        (a, grid') =  runState(changeDisplay cpu x y n) grid --gets the updated state of the grid. 
        setReg0xF True = execState (setReg 0xF 0x1) cpu -- sets the flag to the corresponding value. 
        setReg0xF False = execState (setReg 0xF 0x0) cpu



{-- change Display cpu -> x -> y -> height
    This is the the bulk of the function which does the actual updating of the display, it 
    calls upon a helper function which finds all the pixels which need updating in a single row,
    and then uses it to find what pixels for every row need updating and then updates them. 

    Returns: The updated PixelGrid and the CPU state.
--}
changeDisplay :: CPU -> Word8 -> Word8 -> Word8 -> State  PixelGrid Bool
changeDisplay cpu x y n = do
    grid <- get
    let vx = fromIntegral (getRegVal cpu x) `mod` 64
    let vy = fromIntegral (getRegVal cpu y) `mod` 32 -- wraps round when finding Vx and Vy
    let start = vy * 64 + vx -- 1d coordinate


    -- colour rows gets all of the pixels which need to change
    -- then mapToTuple converts it into a list of tuples of the form (1d coordinate, colour)
    let changes =  mapToTuple grid (colourRows start n 0)

    -- update the grid with the new colours
    put (grid `V.update` changes)
    
    -- uses any to check if any of the pixels were turned off to black
    let bool = V.any (\(_, a) -> a == Black) changes
    return bool

    where
        -- will return the 1d coordinate of all pixels in a single row whose colour needs to be changed.
        colourRows start' height n'
            | start' < 2048 && height /= 0 = colourRow start' n' cpu V.++ colourRows (start' + 64) (height-1) (n'+1)
            | otherwise = V.empty

        -- will return a list of tuples of the 1d coordinate and the colour of the pixel so we can use 
        -- later use the update function to change the colour of each pixel in the vector grid.
        mapToTuple grid v= V.map(\x -> (x, flipColour $ grid V.! x)) v 


{-- colourRow start n cpu
    This is a helper function which finds all the pixels which need to be updated in a single row.
    It does this by finding the bit pattern of the sprite data in memory at the address in the instruction register.
    It then uses the bit pattern to find the 1d coordinate of all pixels which need to be updated. 

    Returns: A vector of tuples of the 1d coordinates of all the pixels that need to be updated.
--}
colourRow :: Int -> Int -> CPU -> V.Vector Int
colourRow start n cpu =   sprite'
    where
        sprite = readMemory (ir cpu + fromIntegral n) cpu
        pos i = fromIntegral (sprite `shiftR` (7 - i) .&. 0x1) -- shifts it to the right to then find whether the bit in that position is a 1 or 0.

        -- Checks it doesn't wrap and the bit is not 1, and then returns the 1d coordinate of the each pixel in the row.
        sprite' =  V.fromList $  [ i  + fromIntegral start + pos i | i <- [0..7], pos i < 64, pos i /= 0 ]



{-------------------------------------------------------------------------------------------------------}


{-- These methods aren't completely tested and I'm not 100% sure if they work as I haven't finished
    everything to be able to run a ROM which would use them, so I can't tell for sure if they all work. 
    The test ROM i provided doesn't us any of these commands -}


{-- callSubroutine address (2NNN)
--  This will set the PC to the address specified (NNN) and then
--  push the address removed from the PC so taht it can be 
-- fetched at a later date with teh returnSubroutine method (00EE)
--
-- returns the CPU state with the PC and stack updated so that in the next
-- FDE cycle the subroutine will be called.
--}
callSubroutine :: Word16 -> State CPU ()
callSubroutine ptr = do
    cpu <- get
    pushStack cpu (pc cpu)
    jump ptr


{-- returnSubroutine (00EE)
--  This will pop the stack and set the PC to the address popped from the stack.
--  so that in the next FDE cycle, the instruction at that address will be executed.
--}
returnSubroutine :: State CPU ()
returnSubroutine = do
    cpu <- get
    popStack cpu >>= setPC

{-- skipIfEqual address value
-- This will increment the PC by 2 if the value of the register at the address
-- is the same as the value passed into the function.
--}
skipIfEqual :: Word8 -> Word8 -> State CPU ()
skipIfEqual ptr val = do
    cpu <- get
    Control.Monad.when (regs cpu V.! fromIntegral ptr == val) $ jump $ pc cpu + 2

{-- skipIfNot Equal address value
-- 
-- This is the exact same as the function above, except now it'll
-- only increment if they are NOT equal. 
--}
skipIfNotEqual :: Word8 -> Word8 -> State CPU ()
skipIfNotEqual ptr val = do
    cpu <- get
    Control.Monad.when (regs cpu V.! fromIntegral ptr /= val) $ jump $ pc cpu + 2

{-- skipIfRegsEqual 
--
--  Similar to before, but now it's checking if the values of two registers
--  are equal, and incrementing the PC if it is the case.
--
--}
skipIfRegsEqual :: Word8 -> Word8 -> State CPU ()
skipIfRegsEqual ptr1 ptr2 = do
    cpu <- get
    Control.Monad.when
        (reg1 cpu == reg2 cpu) $ jump $ pc cpu + 2

    where reg1 cpu= regs cpu V.! fromIntegral ptr1
          reg2 cpu = regs cpu V.! fromIntegral ptr2


{-- skipIfRegsNotEqual
-- 
-- Same as above, except not it'll only increment the PC
-- if they are NOT equal. 
--}
skipIfRegsNotEqual :: Word8 -> Word8 -> State CPU ()
skipIfRegsNotEqual ptr1 ptr2 = do
    cpu <- get
    Control.Monad.when
        (reg1 cpu /= reg2 cpu) $ jump $ pc cpu + 2

    where reg1 cpu= regs cpu V.! fromIntegral ptr1
          reg2 cpu = regs cpu V.! fromIntegral ptr2


{-- setRegLogicOp operation ptr1 ptr2 
--
-- This will get the values from two registers, and then change the
-- value of (only)  the first register by applying the logic operation to the
-- both the values in the registers,  
--
-- I decided to use a generic function which accepts multiple operators,
-- As many of the functions are the exact same, but with different logic operations,
-- Therefore instead of making multiple different functions, I could just use the same
-- one and pass in a different operator. 
--}
setRegLogicOp :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> State CPU()
setRegLogicOp operator ptr1 ptr2  = do
    cpu <- get
    setReg ptr1 (r1 cpu `operator` r2 cpu)
    where
        r1 cpu = getRegVal cpu ptr1
        r2 cpu = getRegVal cpu ptr2





{-- setRegAdd address1 address 2
--
-- This adds the values of two seperate regiseter addresses, and then sets the value of the first 
-- register to it. If over flow occurs, then the flag register VF should be set to 1, otherwise 0.
--}
setRegAdd :: Word8 -> Word8 -> State CPU()
setRegAdd ptr1 ptr2 = do
    cpu <- get
    let r1 = regs cpu V.! fromIntegral ptr1
    let r2 = regs cpu V.! fromIntegral ptr2
    if (fromIntegral  r1 + fromIntegral r2) > 255 then setFlagReg 1 else setFlagReg 0
    setReg ptr1 (r1 + r2)


{-- setRegMinux address1 address 2
--
-- This subtracts the values of two seperate regiseter addresses, and then sets the value of the first 
-- register to it. If under flow occurs, then the flag register VF should be set to 0, otherwise 1.
--}
setRegMinus:: Word8 -> Word8 -> State CPU()
setRegMinus ptr1 ptr2 = do
    cpu <- get
    let r1 = regs cpu V.! fromIntegral ptr1
    let r2 = regs cpu V.! fromIntegral ptr2
    if (fromIntegral  r1 - fromIntegral r2) <0 then setFlagReg 0 else setFlagReg 1
    setReg ptr1 (r1 - r2)

{-- setRegMinusOpposite address1 address 2
--
-- This is the exact same as the function above, except the order in which we subtract the values is 
-- switched around,
--}

setRegMinusOpposite :: Word8 -> Word8 -> State CPU()
setRegMinusOpposite ptr1 ptr2 = do
    cpu <- get
    let r1 = regs cpu V.! fromIntegral ptr1
    let r2 = regs cpu V.! fromIntegral ptr2
    if (fromIntegral  r2 - fromIntegral r1) <0 then setFlagReg 0 else setFlagReg 1
    setReg ptr1 (r2 - r1)


{-- shiftright1 address
-- This takes the value from the register at the address, and then shifts it right by 1,
-- If a bit is shifted out, then the flag register VF should be set to 1, otherwise 0.
--}
shiftRight1 :: Word8 -> State CPU()
shiftRight1 ptr = do
    regs' <- gets regs
    let r1 = regs' V.! fromIntegral ptr
    if (r1 .&. 0x1) == 0x1 then setFlagReg 1 else setFlagReg 0 -- checks if a bit has been shifted out. 
    setReg ptr (r1 `shiftR` 1)


{-- shiftleft1 address
-- This is the exact same as above, except it shifts the value left by 1 instead. 
--}
shiftLeft1 :: Word8 -> State CPU()
shiftLeft1 ptr = do
    regs' <- gets regs
    let r1 = regs' V.! fromIntegral ptr
    if (r1 .&. 0x80) == 0x80 then setFlagReg 1 else setFlagReg 0
    setReg ptr (r1 `shiftL` 1)


{-- Jump Offset address
--  This sets the value of the PC to the value in the address, and also adds the value in the 0x0 register to it. 
--}
jumpOffset :: Word16 -> State CPU ()
jumpOffset address = do
    cpu <- get
    jump $ fromIntegral address + fromIntegral (getRegVal cpu 0x0)




-- {-- random address value
-- --
-- -- This will get the value from the register at the address, and then generate a random number between 0 and 255,
-- -- It
-- --}  
-- random ::  Word8  -> Word8 -> State CPU ()
-- random address val = do
--     cpu <- get
--     let r = randomRIO (0, 255) :: IO Word8
--     put cpu { regs = regs cpu V.// [(fromIntegral address, val)] }
--     return ()

-- addToIR :: Word8 -> State CPU ()
-- addToIR ptr = do
--     cpu <- get
--     put cpu { ir = fromIntegral (regs cpu V.! fromIntegral ptr + fromIntegral (ir cpu)) }



