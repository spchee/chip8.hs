
module CPU.Commands where
import Display.Display

import Data.Word (Word16, Word8)
import CPU.Data ( CPU(ir, regs), getPC, setPC, getRegVal )
import Data.Vector as V ( (//), (!), Vector, replicate )
import Control.Monad.State
import Graphics.Gloss.Data.Picture
import Graphics.Gloss




type Reg = Word8



clear :: V.Vector Colour
clear = V.replicate 2048 Black

jump :: Word16 -> State CPU ()
jump addr = do
    pc <- getPC
    setPC addr

setReg :: Reg -> Word8 -> State CPU  ()
setReg reg val = do
    cpu <- get
    put cpu { regs = regs cpu V.// [(fromIntegral reg, val)] }

addToReg :: Reg -> Word8 -> State CPU ()
addToReg ptrReg val = do
    regs' <- gets regs
    modify $ \c -> c { regs = regs' V.// [(fromIntegral ptrReg, (regs' V.! fromIntegral ptrReg) + val)] }

setIR :: Word16 -> State CPU ()
setIR addr = do
    cpu <- get
    let i = ir cpu
    modify $ \c -> c { ir = addr }


display :: CPU -> PixelGrid -> Word8 -> Word8 -> Word8 -> (PixelGrid, CPU)
display cpu grid x y n = (grid', setReg0xF a)

    where
        vx = getRegVal cpu x
        vy = getRegVal cpu x
        (a, grid') =  runState(changeDisplay cpu x y n) grid
        setReg0xF True = execState (setReg 0xF 0x1) cpu
        setReg0xF False = execState (setReg 0xF 0x0) cpu

changeDisplay :: CPU -> Word8 -> Word8 -> Word8 -> State  PixelGrid Bool
changeDisplay cpu x y n = do
    g <- get
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


getFlips :: CPU -> Word8 -> Word8 -> Word8 -> [Int]
getFlips cpu x y n = getFlips' start' n
    where
        getFlips' start n
            | start < 0 || n <= 0 = []
            | otherwise = fromIntegral start : getFlips' (n-64) (n-1)

        start' = fromIntegral (regs cpu V.! fromIntegral x) * (regs cpu V.! fromIntegral y)





