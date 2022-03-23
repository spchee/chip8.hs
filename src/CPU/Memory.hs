module CPU.Memory where

import Data.Word (Word8, Word16)
import Control.Monad.State ( modify, MonadState(get), State)
import CPU.Data (CPU(memory) )
import qualified Data.Vector as V
import Debug.Trace (traceM)

readMemory :: Word16 -> CPU -> Word8
readMemory i cpu = memory cpu V.! fromIntegral i



writeMemory :: Word16 -> Word8 -> State CPU ()
writeMemory i v = do
    --traceM $ "writeMemory " ++ show i ++ " " ++ show v
    cpu <- get
    let mem = memory cpu
    modify $ \c -> c { memory = mem V.// [(fromIntegral i, v)] }


