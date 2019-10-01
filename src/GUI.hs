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
squareSize = 4

window :: Int -> Int -> Display
window w h = InWindow windowTitle (w * squareSize, h * squareSize) (0, 0)


-- displays a board state as a Picture
-- TODO implement
displayBoard :: Model -> Picture
displayBoard (rs, as, b) = rectangleSolid 4 4


-- TODO implement
step :: ViewPort -> Float -> Model -> Model
step _ _ (rs, as, b) = (rs, as, b)


go :: Model -> IO ()
go (rs, as, b) = simulate (window w h) background fps (rs, as, b) displayBoard step
                 where h = length b
                       w = length (b !! 0)