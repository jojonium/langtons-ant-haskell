module Sim
    ( Model(..)
    , runSim
    , runSimGraphical
    ) where

import Board
import Ant
import Rule
import GUI

type Model = (Ruleset, [Ant], Board)

-- runs the simulation for n steps, returning a list of (ant list, board) pairs
-- representing each step along the way
runSim :: (Integral a) => Model -> a -> [Model]
runSim _ 0 = []
runSim (rs, as, b) n
  | n < 0     = error "steps must be positive"
  | otherwise = let nextTriple = step (rs, as, b)
                 in nextTriple : if length as > 0
                                    then runSim nextTriple (pred n)
                                        else []  -- end if no ants left


runSimGraphical :: (Integral a) => Model -> a -> IO ()
runSimGraphical (rs, as, b) n
  | n < 0     = error "steps must be positive"
  | otherwise = do go (rs, as, b)


-- moves the simulation forward one step, moving each ant and then updating the
-- board. The board must be updated after each ant is finished moving so that
-- multiple ants moving to the same square don't interfere with each other
step :: Model -> Model
step ((wrap, rs), as, b) =
  let height   = length b
      width    = length (b !! 0)
      newAnts  = filter (sane wrap width height) $ moveAnts (wrap, rs) b as
      newBoard = updateColors (wrap, rs) b newAnts
   in ((wrap, rs), newAnts, newBoard)
  where sane wrap w h (Ant _ (Coord ax ay) _) = wrap || (ax >= 0 && ay >= 0 &&
                                                         ax <  w && ay <  h)

-- moves all ants without modifying the board
moveAnts :: Ruleset -> Board -> [Ant] -> [Ant]
moveAnts rs b = map (moveAnt rs b)
    where moveAnt (wrap, rs) b (Ant ad (Coord ax ay) (Coord lx ly)) = -- move single ant
            let inst         = rs !! ((b !! ay) !! ax)
                newD         = turn ad inst  -- turn according to rule on spot
                (dx, dy)     = deltas newD
                height       = length b
                width        = length (b !! 0)
                (newX, newY) = adjustWrap wrap height width (ax + dx, ay + dy)
             in Ant newD (Coord newX newY) (Coord ax ay)


-- if wrap is true, wraps positions that are off the board
adjustWrap :: Bool -> Int -> Int -> (Int, Int) -> (Int, Int)
adjustWrap False _ _ a     = a
adjustWrap True h w (x, y) = (adjust w x, adjust h y)
    where adjust bound val
            | val < 0         = bound - 1
            | val > bound - 1 = 0
            | otherwise       = val


-- increments the colors of all squares an ant just left, without modifying ants
updateColors :: Ruleset -> Board -> [Ant] -> Board
updateColors (wrap, rs) = foldl (updateColor rs)
    where updateColor rs b (Ant _ _ (Coord x y)) =  -- update single color
            let oldCol = (b !! y) !! x
                newCol = mod (succ oldCol) (length rs)
             in updateMatrix b newCol (y, x)


-- replaces the element at (r, c) in the array with a
updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r, c) =
  take r m ++ [take c (m !! r) ++ [x] ++ drop (c + 1) (m !!r)] ++ drop (r + 1) m
