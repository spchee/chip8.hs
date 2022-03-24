{-# LANGUAGE RecordWildCards #-}
module Rom.LoadRom where

import System.Environment
import Data.Word (Word8, Word16)
import Data.List
import qualified Data.ByteString as B
import CPU.Data
import Control.Monad.State
import CPU.Memory
import Debug.Trace

{-Loads a ROM file into a list of integers so it can be loaded into memory.-}
loadFile :: FilePath -> IO [Int]
loadFile path = do
    file <- B.readFile path
    let list = map fromIntegral (B.unpack file)
    return list

{- Loads a ROM file into a list of integers so it can be loaded into memory.
   This function is used to load the ROM into memory. 
   
   All instructions start at 0x200. Since the first 512 bits in a normal chip8 system would 
   be reserved for other things such as registers. But sinec we're emulating it we're keeping it all seperate in seperate variables
   as it makes it easier to manage.
   -}
loadIntoMemory :: [Int] -> State CPU ()
loadIntoMemory binaryList = do 
    cpu@CPU{..} <- get
    writeMemory' (map fromIntegral binaryList) 0x200
    where
      writeMemory' (x:xs) ptrMem  = do
        writeMemory ptrMem x
        writeMemory' xs (ptrMem + 1)
      writeMemory' [] _ = return ()







