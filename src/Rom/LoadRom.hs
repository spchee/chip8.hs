{-# LANGUAGE RecordWildCards #-}
module Rom.LoadRom where

import System.Environment
import Data.Word (Word8, Word16)
import Data.List
import qualified Data.ByteString as B
import CPU.Data
import Control.Monad.State
import CPU.Memory



loadFile :: FilePath -> IO [Int]
loadFile file = do
    content <- B.readFile file
    putStrLn $ "Loaded file: " ++ file
    let binaryList = map fromIntegral (B.unpack content)
  -- print each element in binaryList
    return binaryList

loadIntoMemory :: [Int] -> State CPU ()
loadIntoMemory binaryList = do 
    cpu@CPU{..} <- get
    writeMemory' (map fromIntegral binaryList) 0x200
    where
      writeMemory' (x:xs) ptrMem  = do
        writeMemory ptrMem x
        writeMemory' xs (ptrMem + 1)
      writeMemory' [] _ = return ()






