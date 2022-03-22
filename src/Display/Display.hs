{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Display.Display where
import Graphics.Gloss hiding (scale, Point)
import Graphics.Gloss.Data.Picture hiding (Point, scale)
import Data.Vector as V
import Control.Monad.State
import qualified Control.Applicative as V.Vector
width :: Int
width = 64
height :: Int
height = 32

scale :: Int
scale = 20

data Colour = White | Black
    deriving (Eq, Show)



type PixelGrid = V.Vector Colour



initGrid :: State PixelGrid ()
initGrid =
    put $  V.replicate 2048 Black


alternating :: PixelGrid
alternating = V.generate 2048 (\n -> if even n then White else Black)

pixelGridToPictures :: Int -> PixelGrid -> [Picture]
pixelGridToPictures n grid
    | V.null grid = []
    | otherwise = uncurry translate (getCoords n) (color (getColour grid) $ rectangleSolid (scale'-2) (scale'-2)) :pixelGridToPictures (n+1) (V.drop 1 grid)
    where
        getColour grid
            | V.head grid == White = white
            | V.head grid == Black = black
            | otherwise = error "Invalid colour"
        scale' = fromIntegral scale



getCoords:: Int -> (Float, Float)
getCoords n = (x , y)
    where 
        x = fromIntegral ((n `mod` 64) * scale - 32 * scale + scale `div` 2)
        y = fromIntegral ((n `div` 64) * scale -  16 * scale + scale `div` 2)
-- alternating black and white squares


window :: Display
window = InWindow "Nice Window" ( width', height') (100, 100)
    where 
        width' = scale * width + scale
        height' = scale * height + scale

background :: Color
background = greyN 0.15

main :: IO ()
main = display window background (pictures $ pixelGridToPictures 0 alternating)

-- addPixel :: (Int, Int) -> Picture
-- addPixel (x, y) = translate (fromIntegral x) (fromIntegral y) $ color black $ rectangleSolid 10 10

-- removePixel :: (Int, Int) -> Picture
-- removePixel (x, y) = translate (fromIntegral x) (fromIntegral y) $ color white $ rectangleSolid 10 10



