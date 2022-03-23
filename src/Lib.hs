module Lib
    ( someFunc
    ) where
import CPU.CPU
import Display.Display hiding (window)
import Rom.LoadRom
import Control.Monad.State
import CPU.Data
import Prelude hiding (cycle)
import Control.Concurrent
import Graphics.Gloss (pictures, play, Display (InWindow), Picture)
import Graphics.Gloss.Interface.IO.Game (Event)
import Graphics.Gloss.Data.Color

{- NOTE that you are NOT obligated to keep any of the files from 
the skeleton code, including this one. You should give your 
modules sensible names that correspond to their contents. -}

someFunc :: IO ()
someFunc = do
    file <- loadFile "test.ch8"
    putStrLn "1"
    let cpu = execState (loadIntoMemory file) initCPU
    
    return ()


window :: Display
window = InWindow "Nice Window" ( 640, 320) (100, 100)

updateDisplay :: PixelGrid -> CPU   -> IO ()
updateDisplay grid cpu = play window (greyN 0.15) 100  (initGrid, initCPU) toPicture something nextState 

        
        

toPicture :: (PixelGrid, CPU) -> Picture 
toPicture (grid, _)= pictures (pixelGridToPictures 0 grid)

nextState :: p -> (PixelGrid, CPU) -> (PixelGrid, CPU)
nextState _ (grid, cpu) = execute grid cpu (fetch cpu)


something :: Event -> world -> world
something _ world = world
-- cycle :: CPU -> PixelGrid -> 
-- cycle cpu grid = do
--     let (newGrid, newCpu) = 

--     return ()