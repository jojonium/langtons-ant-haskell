module GUI
    ( go
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
background =  white

squareSize :: Int
squareSize = 10

window :: Int -> Int -> Display
window w h = InWindow windowTitle (width, height) (0, 0)
             where width  = w * squareSize
                   height = h * squareSize


-- displays a board state as a Picture
-- TODO implement
displayBoard :: Model -> Picture
displayBoard (_, _, b) = 
  Pictures $ map drawOne [(x, y) | x <- [0..w-1], y <- [0..h-1]]
  where ss = fromIntegral squareSize
        h  = length b
        w  = length (b !! 0)
        drawOne :: (Int, Int) -> Picture
        drawOne (x, y) = drawSquare x y w h (0)


drawSquare :: Int -> Int -> Int -> Int -> Int -> Picture
drawSquare x y w h n =
  Translate a b $ rectangleSolid ss ss
  where ss = fromIntegral squareSize
        a  = getOffsetX x w
        b  = getOffsetY y h


getOffsetX :: Int -> Int -> Float
getOffsetX x w = (fromIntegral (x - (div w 2)) + 0.5) * ss
                 where ss:: Float
                       ss = fromIntegral squareSize


getOffsetY :: Int -> Int -> Float
getOffsetY y h = (fromIntegral (y + (div h 2)) - 0.5) * ss
                 where ss:: Float
                       ss = fromIntegral squareSize


-- TODO implement
step :: ViewPort -> Float -> Model -> Model
step _ _ (rs, as, b) = (rs, as, b)


go :: Model -> IO ()
go (rs, as, b) = 
  simulate (window w h) background fps (rs, as, b) displayBoard step
  where h = length b
        w = length (b !! 0)