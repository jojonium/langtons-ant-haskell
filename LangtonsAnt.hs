module Main ( main ) where

import Control.Concurrent
import System.IO
import System.Environment
import Data.Maybe ( fromMaybe )
import System.Exit
import System.Console.GetOpt
import Board
import Ant
import Rule
import Sim


---------------------------- Command Line Arguments ----------------------------

data Options = Options
    { optAntStr  :: String
    , optRuleStr :: String
    , optHeight  :: Int
    , optWidth   :: Int
    , optWrap    :: Bool
    , optVersion :: Bool
    , optHelp    :: Bool
    } deriving (Show)

defaultOptions = Options
    { optAntStr  = "(20, 20 Up)"
    , optRuleStr = "TurnLeft, TurnRight"
    , optHeight  = 50
    , optWidth   = 50
    , optWrap    = False
    , optVersion = False
    , optHelp    = False
    }

options :: [OptDescr (Options -> Options)]
options =  -- I'm sorry this is so ugly
    [ Option ['a'] ["ants"]
          (ReqArg (\ s opts -> opts { optAntStr = s }) "ANTSTR")
          "specify ant string, like '(20, 20, Up), (15, 10, Left)'"
    , Option ['r'] ["rules"]
          (ReqArg (\ s opts -> opts { optRuleStr = s }) "RULESTR")
          "specify rule string, like 'TurnLeft, TurnRight'"
    , Option ['h'] ["height"]
          (ReqArg (\ i opts -> opts { optHeight = read i }) "N")
          "specify board height"
    , Option ['w'] ["width"]
          (ReqArg (\ i opts -> opts { optWidth = read i }) "M")
          "specify board width"
    , Option ['p'] ["wrap"]
          (NoArg (\ opts -> opts { optWrap = True }))
          "ants wrap around board edges"
    , Option ['v'] ["version"]
          (NoArg (\ opts -> opts { optVersion = True }))
          "output version information and exit"
    , Option []    ["help"]
          (NoArg (\ opts -> opts { optHelp = True }))
          "display this help and exit"
    ]


main = getArgs >>= parse >>= print

parse argv = 
  case getOpt Permute options argv of
    (o, _, [])   -> return (process (foldl (flip id) defaultOptions o))
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo h options))
        where h = "Usage: LangtonsAnt [OPTION]..."

process :: Options -> (Ruleset, [Ant], Board)
process o = (testRules, testAnts, testBoard)


version = putStrLn "LangtonsAnt version 0.0.1\nWritten by Joseph Petitti"
success = exitWith ExitSuccess
fail    = exitWith (ExitFailure 1)

--------------------------------------------------------------------------------


testBoard :: Board
testBoard = emptyBoard 50 50

testRules :: Ruleset
testRules = ( True
            , [ Rule TurnRight "white"
              , Rule TurnLeft "black"
              ]
            )

testAnts :: [Ant]
testAnts = [ newAnt 25 25 Up
           ]

-- prints each board along the way
printTest :: Int -> IO ()
printTest n = let x = runSim testRules (testAnts, testBoard) n
                  y = map snd x
               in printBoards y

-- prints only the last board
printTestResult :: Int -> IO()
printTestResult n = let x = runSim testRules (testAnts, testBoard) n
                        y = snd (last x)
                     in printBoard y

printBoards :: [Board] -> IO ()
printBoards bs = mapM_ printBoard bs 

printBoard :: Board -> IO ()
printBoard b = putStrLn $ '\n' : stringify b
