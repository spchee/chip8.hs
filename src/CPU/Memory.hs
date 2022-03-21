module CPU.Memory where

import Data.Word (Word8, Word16)
import Control.Monad.State ( modify, MonadState(get) )
import CPU.Data ( CPUState, CPU(memory) )
import qualified Data.Vector as V

readMemory :: Word16 -> CPUState Word8
readMemory i = do
    cpu <- get
    let mem = memory cpu
    return $ mem V.! fromIntegral i

writeMemory :: Word16 -> Word8 -> CPUState ()
writeMemory i v = do
    cpu <- get
    let mem = memory cpu
    modify $ \c -> c { memory = mem V.// [(fromIntegral i, v)] }