module CPU.Data where
import Data.Word (Word16, Word8)
import Data.Vector as V (Vector, (!), (//))
import Control.Monad.State

data CPU = CPU {
    pc :: Word16,
    ir:: Word16,
    memory :: V.Vector Word8,
    stack :: V.Vector Word16,
    regs :: V.Vector Word8

} deriving (Show)

type CPUState = State CPU

getPC :: CPUState Word16
getPC = gets pc

setPC :: Word16 -> CPUState ()
setPC pc' = modify $ \cpu -> cpu { pc = pc' }

getIR :: CPUState Word16
getIR = gets ir

setIR :: Word16 -> CPUState ()
setIR ir' = modify $ \cpu -> cpu { ir = ir' }

setRegVal :: Word16 -> Word8 -> CPUState ()
setRegVal val reg = do
    cpu <- get
    put cpu { stack = stack cpu V.// [(fromIntegral reg, val)] }

getRegVal :: Word16 -> CPUState Word8
getRegVal i = do
    cpu <- get
    return $ regs cpu V.! fromIntegral i

