{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Display.Display where
import Graphics.Gloss hiding (scale, Point)
import Graphics.Gloss.Data.Picture hiding (Point, scale)
import Data.Vector as V
import Control.Monad.State
import qualified Control.Applicative as V.Vector
import Graphics.Gloss.Interface.IO.Game (Event)

width :: Int
width = 64
height :: Int
height = 32


-- I decided to set actual values for the width, height and scale, as it meants it
-- can easily be adjusted to any size depending on the resolution and size of the screen.
-- A future thing I might do is have a terminal that allows people to schoose the size of the screen

scale :: Int
scale = 20

-- The colours which we use for the game and how we represent them in our vectors. 
data Colour =  White | Black
    deriving (Eq, Show)

flipColour :: Colour -> Colour
flipColour White = Black
flipColour Black = White

{-- I decided to make the grid of pixels a single vector, as it makes it easier to
    manipulate the pixels in the vector with it being a single vector and it'd be slightly more efficient 
    when doing certain operations. --}
type PixelGrid = V.Vector Colour





{-- This converts a vector of pixels to a picture for gloss to display on the screen.-
It loops through each pixel using tail recursion whils adjusting the position on the screen each time. 
(notice the translate getCoordsn, and how n is incremented by 1 each time to account for it being a new pixel)

-}

pixelGridToPictures :: Int -> PixelGrid -> [Picture]
pixelGridToPictures n grid
    | V.null grid = []
    | otherwise = uncurry translate (getCoords n) (color (getColour grid) $ rectangleSolid scale' scale') : pixelGridToPictures (n+1) (V.drop 1 grid)
    where
        getColour grid
            | V.head grid == White = white
            | V.head grid == Black = black
            | otherwise = error "Invalid colour"
        scale' = fromIntegral scale


{-- getCoords given 1d coordinate index,  it'll efficiently convert them into a 2d coordinate which gloss can use to put it on the screen.
In gloss coordinates start from the middle of the screen. However this is really annoying and counter intuitive, but this converts it so that 
the grid of pixels start from the top left like literally everything else
--}
getCoords:: Int -> (Float, Float)
getCoords n = (x , y)
    where
        x = fromIntegral ((n `mod` 64) * scale - 32 * scale + scale `div` 2) 
        y = fromIntegral (-(n `div` 64) * scale +  16 * scale - scale `div` 2) -- div rounds down so it'll always be the correct integer height. 
-- alternating black and white squares




background :: Color
background = greyN 0.15

-- animateDisplay  ::PixelGrid -> Float -> IO ()
-- animateDisplay grid t = display window background (pictures $ pixelGridToPictures 0 grid) 


updateDisplay :: a -> (a -> Picture) -> (Event -> a -> a) -> (Float -> a -> a) -> IO()
updateDisplay = play window background 5


window :: Display
window = InWindow beeMovieScript ( width', height') (100, 100)
    where
        width' = scale * width + scale
        height' = scale * height + scale


beeMovieScript :: String 
beeMovieScript = "According to all known laws of aviation, there is no way a bee should be able to fly."
