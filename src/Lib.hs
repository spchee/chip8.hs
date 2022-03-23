module Lib
    ( someFunc
    ) where
import CPU.CPU
import Display.Display 
import Rom.LoadRom
import Control.Monad.State
import CPU.Data
import Prelude hiding (cycle)
import Control.Concurrent
import Graphics.Gloss (pictures, play, Display (InWindow), Picture)
import Graphics.Gloss.Interface.IO.Game (Event)
import Graphics.Gloss.Data.Color
import Debug.Trace



{- NOTE that you are NOT obligated to keep any of the files from 
the skeleton code, including this one. You should give your 
modules sensible names that correspond to their contents. -}

someFunc :: IO ()
someFunc = do
    file <- loadFile "test.ch8"
    traceM "1"
    let cpu = execState (loadIntoMemory file) initCPU
    traceM "2"
    updateDisplay'  (initGrid, cpu)
    traceM "3"
    return ()





updateDisplay' :: (PixelGrid, CPU) -> IO ()
updateDisplay' initial = updateDisplay initial toPicture (const id) nextState 

nextState :: p -> (PixelGrid, CPU) -> (PixelGrid, CPU)
nextState _ (grid, cpu) = uncurry (execute grid)  op
    where op = fetch cpu 


toPicture :: (PixelGrid, CPU) -> Picture
toPicture (grid, _) = pictures (pixelGridToPictures 0 grid)



something :: Event -> world -> world
something _ world = world
-- cycle :: CPU -> PixelGrid -> 
-- cycle cpu grid = do
--     let (newGrid, newCpu) = 

--     return ()