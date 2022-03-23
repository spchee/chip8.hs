module CPU.Data where
import Data.Word (Word16, Word8)
import Data.Vector as V (Vector, (!), (//))
import Control.Monad.State
import Control.Monad

data CPU = CPU {
    pc :: Word16,
    ir:: Word16,
    memory :: V.Vector Word8,
    stack :: V.Vector Word16,
    regs :: V.Vector Word8,
    delay_timer :: Word8,
    sound_timer :: Word8

} deriving (Show)

decrementDelayTimer ::  State CPU ()
decrementDelayTimer = do
    delay_timer <- gets delay_timer
    Control.Monad.when (delay_timer > 0) $ do
        modify $ \cpu -> cpu {delay_timer = delay_timer - 1}

getPC :: State CPU Word16
getPC = gets pc

setPC :: Word16 -> State CPU ()
setPC pc' = modify $ \cpu -> cpu { pc = pc' }

setRegVal :: Word8 -> Word8 -> State CPU ()
setRegVal val reg = do
    cpu <- get
    put cpu { regs = regs cpu V.// [(fromIntegral reg, val)] }

getRegVal :: CPU -> Word8-> Word8
getRegVal cpu ptr = regs cpu V.! fromIntegral ptr



