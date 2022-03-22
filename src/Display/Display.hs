{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Display.Display where
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Data.Picture hiding (Point)
import Data.Vector as V
import Control.Monad.State
width :: Integer
width = 64
height :: Integer
height = 32

data Colour = White | Black
    deriving (Eq, Show)

type GridState = State (V.Vector Colour)

initGrid :: GridState ()
initGrid = do
    put $ V.replicate 2048 Black

getCoords:: Int -> (Int, Int)
getCoords n = (10 * (n `mod` 64), (n `div` 32) * 10)

squares :: [Picture]
squares =  [color white $ rectangleSolid x y | x <- [0..5], y <- [0..5]]


background :: Color
background = black


window :: Display
window = InWindow "Nice Window" (640, 320) (100, 100)

main :: IO ()
main = display window background (pictures squares)

addPixel :: (Int, Int) -> Picture
addPixel (x, y) = translate (fromIntegral x) (fromIntegral y) $ color black $ rectangleSolid 10 10

removePixel :: (Int, Int) -> Picture
removePixel (x, y) = translate (fromIntegral x) (fromIntegral y) $ color white $ rectangleSolid 10 10



