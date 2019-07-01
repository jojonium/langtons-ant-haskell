module Board
    ( Coord(..)
    , Board
    , emptyBoard
    , stringify
    ) where

import Data.List

data Coord = Coord Int Int deriving (Show)
type Board = [[Int]] -- list of rows


-- creates a w by h board filled with 0s
emptyBoard :: Int -> Int -> Board
emptyBoard w h
  | w < 1 || h < 1 = error "Board width and height must be positive"
  | otherwise      = take h (repeat (take w (repeat 0)))


-- converts a Board to a string with each cell printed as the base64 character
-- of its value and a newline between rows
stringify :: Board -> String
stringify = (intercalate "\n") . map (map toBase64)


toBase64 :: Int -> Char
toBase64 x = b64Chars !! x
    where b64Chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/"

