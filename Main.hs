import Board
import Ant
import Rule

testBoard :: Board
testBoard = emptyBoard 100 100

testRules :: [Rule]
testRules = [Rule TurnRight "white", Rule TurnLeft "black"]

testAnts :: [Ant]
testAnts = [ newAnt 50 50 Up
           , newAnt 25 25 Down ]


printTest :: Int -> IO ()
printTest n = let x = runSim testRules (testAnts, testBoard) n
                  y = map snd x
               in printBoards y

printTestResult :: Int -> IO()
printTestResult n = let x = runSim testRules (testAnts, testBoard) n
                        y = snd (last x)
                     in printBoard y

printBoards :: [Board] -> IO ()
printBoards bs = do mapM_ printBoard bs


printBoard :: Board -> IO ()
printBoard b = do putStrLn $ '\n' : stringify b



-- runs the simulation for n steps, returning a list of (ant list, board) pairs
-- representing each step along the way
runSim :: [Rule] -> ([Ant], Board) -> Int -> [([Ant], Board)]
runSim _ (as, b) 0 = []
runSim rs (as, b) n
  | n < 0     = error "steps must be positive"
  | otherwise = let nextTuple = step rs (as, b)
                 in nextTuple : if length as > 0
                                    then runSim rs nextTuple (pred n)
                                        else []  -- end if no ants left


-- moves the simulation forward one step, moving each ant and then updating the
-- board. The board must be updated after each ant is finished moving so that
-- multiple ants moving to the same square don't interfere with each other
step :: [Rule] -> ([Ant], Board) -> ([Ant], Board)
step rs (as, b) =
  let height   = length b
      width    = length (b !! 0)
      newAnts  = filter (sane width height) $ moveAnts rs b as
      newBoard = updateColors rs b newAnts
   in (newAnts, newBoard)
  where sane w h (Ant _ (Coord ax ay) _) = ax >= 0 && 
                                           ay >= 0 &&
                                           ax <  w && 
                                           ay <  h

-- moves all ants without modifying the board
moveAnts :: [Rule] -> Board -> [Ant] -> [Ant]
moveAnts rs b = map (moveAnt rs b)
    where moveAnt rs b (Ant ad (Coord ax ay) (Coord lx ly)) = -- move single ant
            let Rule inst _  = rs !! ((b !! ay) !! ax)  -- colors don't matter
                newD         = turn ad inst  -- turn according to rule on spot
                (dx, dy)     = deltas newD
                (newX, newY) = (ax + dx, ay + dy)  -- new position
             in Ant newD (Coord newX newY) (Coord ax ay)


-- increments the colors of all squares an ant just left, without modifying ants
updateColors :: [Rule] -> Board -> [Ant] -> Board
updateColors rs = foldl (updateColor rs)
    where updateColor rs b (Ant _ _ (Coord x y)) =  -- update single color
            let oldCol = (b !! y) !! x
                newCol = mod (succ oldCol) (length rs)
             in updateMatrix b newCol (y, x)


-- replaces the element at (r, c) in the array with a
updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r, c) =
  take r m ++ [take c (m !! r) ++ [x] ++ drop (c + 1) (m !!r)] ++ drop (r + 1) m
