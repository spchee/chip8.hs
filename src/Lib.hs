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
import CPU.Commands (clear)



{- NOTE that you are NOT obligated to keep any of the files from 
the skeleton code, including this one. You should give your 
modules sensible names that correspond to their contents. -}


{-- This is the first proper function which is run. 
-- It loads the ROM, initializes the CPU, and starts the main loop. 
--}
someFunc :: IO ()
someFunc = do
    file <- loadFile "test.ch8"
    let cpu = execState (loadIntoMemory file) initCPU
    updateDisplay'  (clear, cpu)
    return ()




{-- updateDisplay 
-- This is an infinite loop which updates the display every cycle.
-- 
-- It has the function nextState, which runs the fetch decode execute cycle and returns the next state of the CPU and grid.
-- the function toPicture converts the grid into a picture which gloss can then actually use to display an image on the screen.
-- At the moment, it doesn't have the ability to handle events (hence why there's the const id)
--}
updateDisplay' :: (PixelGrid, CPU) -> IO ()
updateDisplay' initial = updateDisplay initial toPicture (const id) nextState 
    where 
        nextState _ (grid, cpu) = uncurry (execute grid) $ fetch cpu 
        toPicture (grid, _) = pictures (pixelGridToPictures 0 grid)

