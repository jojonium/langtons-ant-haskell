module GUI
    ( window
    , background
    , fps
    , displayBoard
    ) where

import Ant
import Board
import Rule

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Model = (Ruleset, [Ant], Board)

fps :: Int
fps = 60

windowTitle :: String
windowTitle = "Langton's Ant"

background :: Color
background = makeColorI 230 230 230 255

squareSize :: Int
squareSize = 5

window :: Int -> Int -> Display
window w h = InWindow windowTitle (width, height) (0, 0)
             where width  = w * squareSize
                   height = h * squareSize


-- displays a board state as a Picture
displayBoard :: Model -> Picture
displayBoard (_, _, b) = 
  Pictures $ map drawOne [(x, y) | x <- [0..w-1], y <- [0..h-1]]
  where ss = fromIntegral squareSize
        h  = length b
        w  = length (b !! 0)
        drawOne :: (Int, Int) -> Picture
        drawOne (x, y) = drawSquare x y w h ((b !! y) !! x)


drawSquare :: Int -> Int -> Int -> Int -> Int -> Picture
drawSquare x y w h n =
  Translate a b $ Color (getColor n) $ rectangleSolid ss ss
  where ss = fromIntegral squareSize
        a  = getOffsetX x w
        b  = getOffsetY y h


getOffsetX :: Int -> Int -> Float
getOffsetX x w = (fromIntegral (x - (div w 2)) + 0.5) * ss
                 where ss:: Float
                       ss = fromIntegral squareSize


getOffsetY :: Int -> Int -> Float
getOffsetY y h = (fromIntegral (y - (div h 2)) + 0.5) * ss
                 where ss:: Float
                       ss = fromIntegral squareSize


getColor :: Int -> Color
getColor 0 = white
getColor n
  | n < 0 || n > 63 = error "getColor: n must be [0..63] inclusive"
  | otherwise       = makeColor r g b 1
  where r = (fromIntegral (mod n 5) / 5) :: Float
        g = (fromIntegral (mod n 4) / 4) :: Float
        b = (fromIntegral (mod n 3) / 3) :: Float
