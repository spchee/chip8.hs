module Rom.LoadRom where

import System.Environment
import Data.Word (Word8)
import Data.List
import qualified Data.ByteString as B
import CPU.Data


loadFile :: FilePath -> IO [Int]
loadFile file = do
    content <- B.readFile file
    putStrLn $ "Loaded file: " ++ file
    let binaryList = map fromIntegral (B.unpack content)
  -- print each element in binaryList
    return binaryList


-- loadIntoMemory :: [Int] -> CPUState ()
-- loadIntoMemory content i = do





main :: IO ()
main = do
    content <- loadFile "test.ch8"
    putStrLn "Done"