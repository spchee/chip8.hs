module CPU.Commands where
import Display.Display
import CPU.CPU
import Data.Word (Word16, Word8)
import CPU.Data
import Data.Vector as V ( (//), (!), Vector, replicate )
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

clear :: V.Vector Colour
clear = V.replicate 2048 Black

jump :: Word16 -> CPUState ()
jump addr = do
    pc <- getPC
    setPC addr

setReg :: Reg -> Word8 -> CPUState  ()
setReg reg val = do
    cpu <- get
    put cpu { regs = regs cpu V.// [(fromIntegral reg, val)] }

addToReg :: Reg -> Word8 -> CPUState ()
addToReg ptrReg val = do
    cpu <- get
    let regs' = regs cpu
    modify $ \c -> c { regs = regs' V.// [(fromIntegral ptrReg, (regs' V.! fromIntegral ptrReg) + val)] }

setIR :: Word16 -> CPUState ()
setIR addr = do
    cpu <- get
    let i = ir cpu
    modify $ \c -> c { ir = addr }

test1:: V.Vector Colour 
test1 = V.replicate 1 Black

display :: CPU -> Word16 -> Word16 -> Word8 -> Bool
display cpu x y n =
    -- first value of x == true
    if evalState(changeDisplay cpu x y n) test1
    then True 
    else False



changeDisplay :: CPU -> Word16 -> Word16 -> Word8 -> GridState Bool
changeDisplay cpu x y n = do
    g <- get

    -- let colours' = g V.// [(fromIntegral x + fromIntegral y * 64, n)]

    -- unpack g to get its raw value


    let flips = getFlips cpu x y n

    put (g V.// flipPixels flips g)
    -- if any of the second value equal white
    return $ any (\(_,a) -> a == Black) (flipPixels flips g)



    where
        flipPixels :: [Int] -> V.Vector Colour -> [(Int, Colour)]
        flipPixels (x:xs) grid = (x, flip $ grid V.! x) : flipPixels xs grid
        flipPixels [] grid = []

        flip :: Colour -> Colour
        flip Black = White
        flip White = Black






getFlips :: CPU -> Word16 -> Word16 -> Word8 -> [Int]
getFlips cpu x y n = getFlips' start' n
    where
        getFlips' start n
            | start < 0 || n <= 0 = []
            | otherwise = fromIntegral start : getFlips' (n-64) (n-1)

        start' = fromIntegral (regs cpu V.! fromIntegral x) * (regs cpu V.! fromIntegral y)





