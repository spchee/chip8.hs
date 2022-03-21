module CPU.Commands where
import Display.Display
import CPU.CPU
import Data.Word (Word16, Word8)
import CPU.Data
import Data.Vector as V ( (//), (!), Vector )
import Control.Monad.State
import Graphics.Gloss.Data.Picture
import Graphics.Gloss


type Reg = Word8

data Instruction =
      Clear -- 00E0
    | Jump !Word16 -- 1NNN
    | SetReg !Reg !Word8 --6XNN
    | AddToReg !Reg !Word8 --7XNN
    | SetIR !Word16 -- ANNN
    | Draw !Word16 --DXYN

clear :: [Colour]
clear = replicate 2048 Black

jump :: Word16 -> CPUState ()
jump addr = do
    pc <- getPC
    setPC addr

setReg :: Word8 -> Reg -> CPUState ()
setReg reg val = do
    cpu <- get
    let regs' = regs cpu
    modify $ \c -> c { regs = regs' V.// [(fromIntegral reg, val)] }

addToReg :: Reg -> Word8 -> CPUState ()
addToReg reg val = do
    cpu <- get
    let regs' = regs cpu
    let val' = (regs' V.! fromIntegral reg) + val
    modify $ \c -> c { regs = regs' }

setIR :: Word16 -> CPUState ()
setIR addr = do
    cpu <- get
    let i = ir cpu
    modify $ \c -> c { ir = addr }

display :: Word16 -> Word16 -> n -> GridState Int
display x y n = do
    g <- get 
    -- unpack g to get its raw value
    let g' = unpack g

    let flips = getFlips x y 
    -- for each flip, flip the pixel in grid
    let g' = flipPixels flips g
    return 0
    where 
        flipPixels :: [Int] -> V.Vector Colour -> [(Int, Colour)]
        flipPixels (x:xs) grid = (x, flip $ grid V.! x) : flipPixels xs grid
        flipPixels [] grid = []
        
        flip :: Colour -> Colour 
        flip Black = White
        flip White = Black


    


getFlips :: Word16 -> Word16 -> Int -> CPUState [Int]
getFlips x y n = do
    cpu <- get
    x' <- getRegVal x 
    y' <- getRegVal y 
    let x'' =  fromIntegral x' `mod` 64
    let y'' =  fromIntegral y' `mod` 32
    let changes = getFlips' (x'' * y'') n
    return changes
    where
        getFlips' start n 
            | start < 0 || n <= 0 = []
            | otherwise = start : getFlips' (n-64) (n-1)









    







