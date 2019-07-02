module Ant
    ( Ant(..)
    , Dir(..)
    , newAnt
    , leftOf
    , rightOf
    , deltas
    ) where

import qualified Board as B

data Dir = Up | Down | Left | Right deriving (Show)
data Ant = Ant { dir  :: Dir
               , loc  :: B.Coord
               , prev :: B.Coord
               } deriving (Show)


newAnt :: Int -> Int -> Dir -> Ant
newAnt x y d
  | x < 1 || y < 1 = error "Ant x and y must be positive"
  | otherwise      = Ant {dir = d, loc = B.Coord x y, prev = B.Coord x y}


rightOf :: Dir -> Dir
rightOf d = case d of Ant.Up    -> Ant.Right
                      Ant.Right -> Ant.Down
                      Ant.Down  -> Ant.Left
                      Ant.Left  -> Ant.Up


leftOf :: Dir -> Dir
leftOf d = case d of Ant.Up    -> Ant.Left
                     Ant.Right -> Ant.Up
                     Ant.Down  -> Ant.Right
                     Ant.Left  -> Ant.Down


deltas :: Dir -> (Int, Int)
deltas Ant.Up    = (0, -1)
deltas Ant.Down  = (0, 1)
deltas Ant.Left  = (-1, 0)
deltas Ant.Right = (1, 0)
