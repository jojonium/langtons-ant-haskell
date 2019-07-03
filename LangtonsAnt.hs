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
    , optIter    :: Integer
    , optVersion :: Bool
    , optHelp    :: Bool
    } deriving (Show)

defaultOptions = Options
    { optAntStr  = "(20, 20 Up)"
    , optRuleStr = "TurnLeft, TurnRight"
    , optHeight  = 50
    , optWidth   = 50
    , optWrap    = False
    , optIter    = 1000
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
    , Option ['n'] ["number"]
          (ReqArg (\ i opts -> opts { optIter = read i }) "X")
          "specify number of iterations"
    , Option ['v'] ["version"]
          (NoArg (\ opts -> opts { optVersion = True }))
          "output version information and exit"
    , Option []    ["help"]
          (NoArg (\ opts -> opts { optHelp = True }))
          "display this help and exit"
    ]


main = getArgs >>= parse


parse :: [String] -> IO ()
parse argv = 
  case getOpt Permute options argv of
    (o, _, []) -> do let co = foldl (flip id) defaultOptions o
                     if optHelp co
                        then do putStrLn (usageInfo header options)
                                exitWith ExitSuccess
                        else if optVersion co
                             then do putStrLn version
                                     exitWith ExitSuccess
                             else do processOptions co 
                                     exitWith ExitSuccess
    (_, _, errs) -> do hPutStrLn stderr e
                       exitWith (ExitFailure 1)
      where e = (concat errs ++ usageInfo header options)


processOptions :: Options -> IO ()
processOptions o =
  let as = decodeAntStr (optAntStr o)
      rs = (optWrap o, decodeRuleStr (optRuleStr o))
      w  = optWidth o
      h  = optHeight o
      n  = optIter o
   in do putStrLn $ errorCheck as rs w h n


errorCheck :: [Ant] -> Ruleset -> Int -> Int -> Integer -> String
errorCheck [] _ _ _ _      = error "Couldn't decode ant string"
errorCheck _ (_, []) _ _ _ = error "Couldn't decode rule string"
errorCheck as rs w h n
  | w < 1     = error $ "Width too small: " ++ (show w)
  | h < 1     = error $ "Height too small: " ++ (show h)
  | n < 0     = error $ "Iteration count can't be negative: " ++ show n
  | otherwise = "success" -- TODO


-- converts rule string to list of Rules
decodeRuleStr :: String -> [Rule]
decodeRuleStr [] = error "decodeRuleStr received an empty string"
decodeRuleStr s  = [ TurnLeft ] --TODO


-- converts ant string to list of ants
decodeAntStr :: String -> [Ant]
decodeAntStr [] = error "decodeAntStr received an empty string"
decodeAntStr s  = [ newAnt 50 50 Up ] -- TODO


-- splits a string by delimeter d
split :: Char -> String -> [String]
split d [] = [""]
split d (x:xs)
  | x == d    = "" : rest
  | otherwise = (x : head rest) : tail rest
      where rest = split d xs


version = "LangtonsAnt version 0.0.1\nWritten by Joseph Petitti"
header  = "Usage: LangtonsAnt [OPTION]..."
success = exitWith ExitSuccess
fail    = exitWith (ExitFailure 1)

--------------------------------------------------------------------------------


testBoard :: Board
testBoard = emptyBoard 50 50

testRules :: Ruleset
testRules = ( True
            , [ TurnRight
              , TurnLeft
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
