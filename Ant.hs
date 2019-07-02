module Ant
    ( Ant(..)
    , Dir(..)
    , prevDir
    , nextDir
    , newAnt
    , deltas
    ) where

import qualified Board as B

data Dir = Up | Right | Down | Left deriving (Show, Enum)
data Ant = Ant { dir  :: Dir
               , loc  :: B.Coord
               , prev :: B.Coord
               } deriving (Show)


newAnt :: Int -> Int -> Dir -> Ant
newAnt x y d
  | x < 1 || y < 1 = error "Ant x and y must be positive"
  | otherwise      = Ant {dir = d, loc = B.Coord x y, prev = B.Coord x y}


-- handles wraparound
prevDir :: Dir -> Dir
prevDir Up = Ant.Left
prevDir d  = pred d


-- handles wraparound
nextDir :: Dir -> Dir
nextDir Ant.Left = Up
nextDir d    = succ d


deltas :: Dir -> (Int, Int)
deltas Ant.Up    = (0, -1)
deltas Ant.Down  = (0, 1)
deltas Ant.Left  = (-1, 0)
deltas Ant.Right = (1, 0)
