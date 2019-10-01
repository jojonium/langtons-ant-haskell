module Main ( main ) where

import System.IO             ( IO(..), putStrLn, hPutStrLn, stderr )
import System.Exit           ( exitWith, ExitCode(..) )
import System.Environment    ( getArgs )
import System.Console.GetOpt ( getOpt, OptDescr(..), ArgDescr(..)
                             , ArgOrder(..), usageInfo )
import Data.Maybe            ( fromMaybe )
import Data.Char             ( toLower )
import Data.List             ( isPrefixOf, intercalate )

import Board
import Ant
import Rule
import Sim


data Options = Options
    { optAntStr  :: String
    , optRuleStr :: String
    , optHeight  :: Int
    , optWidth   :: Int
    , optWrap    :: Bool
    , optIter    :: Integer
    , optVersion :: Bool
    , optHelp    :: Bool
    , optGUI     :: Bool
    } deriving (Show)

defaultOptions = Options
    { optAntStr  = "50,50,Up"
    , optRuleStr = "TurnLeft, TurnRight"
    , optHeight  = 100
    , optWidth   = 100
    , optWrap    = False
    , optIter    = 11000
    , optVersion = False
    , optHelp    = False
    , optGUI     = False
    }

options :: [OptDescr (Options -> Options)]
options =  -- I'm sorry this is so ugly
    [ Option ['a'] ["ants"]
          (ReqArg (\ s opts -> opts { optAntStr = s }) "ANTSTR")
          "specify ant string, like '20,20,Up, 15,10,Left'"
    , Option ['r'] ["rules"]
          (ReqArg (\ s opts -> opts { optRuleStr = s }) "RULESTR")
          "specify rule string, like 'TurnLeft, Continue, UTurn'"
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
    , Option ['g'] ["graphical"]
          (NoArg (\ opts -> opts { optGUI = True }))
          "display steps in a graphical window"
    , Option ['v'] ["version"]
          (NoArg (\ opts -> opts { optVersion = True }))
          "output version information and exit"
    , Option []    ["help"]
          (NoArg (\ opts -> opts { optHelp = True }))
          "display this help and exit"
    ]


main = getArgs >>= parse


version = "langtons-ant version 1.1.0\nWritten by Joseph Petitti"
header  = "Usage: langtons-ant [OPTION]..."


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
                             else do runWithOptions co 
                                     exitWith ExitSuccess
    (_, _, errs) -> do hPutStrLn stderr e
                       exitWith (ExitFailure 1)
      where e = (concat errs ++ usageInfo header options)


-- extracts options and runs the simulation to that step, printing the result
runWithOptions :: Options -> IO ()
runWithOptions o =
  let as = decodeAntStr (optAntStr o)
      rs = (optWrap o, decodeRuleStr (optRuleStr o))
      w  = optWidth o
      h  = optHeight o
      n  = optIter o
      g  = optGUI o
   in if not (errorCheck as rs w h n)
         then exitWith (ExitFailure 1)
         else if (g)
                 then runSimGraphical (rs, as, emptyBoard w h) n
                 else let x = runSim (rs, as, emptyBoard w h) n
                          y = thrd (last x)
                              where thrd (_, _, q) = q
                      in putStrLn $ prettyPrintBoard as rs h w n y


-- makes sure inputs are valid, returning true if they are or crashing with an
-- error otherwise. There's probably a more elegant way of doing this with
-- exceptions, but for now this works
errorCheck :: [Ant] -> Ruleset -> Int -> Int -> Integer -> Bool
errorCheck [] _ _ _ _      = error "Couldn't decode ant string"
errorCheck _ (_, []) _ _ _ = error "Couldn't decode rule string"
errorCheck as (_, rs) w h n
  | w < 1         = error $ "Width too small: " ++ (show w)
  | h < 1         = error $ "Height too small: " ++ (show h)
  | n < 0         = error $ "Iteration count can't be negative: " ++ (show n)
  | length rs < 2 = error $ "Too few rules: " ++ show (length rs)
  | otherwise = checkAnts w h as


-- returns True if all ant positions are within the width and height bounds
checkAnts :: Int -> Int -> [Ant] -> Bool
checkAnts w h []     = False;
checkAnts w h as     = foldl (\acc x -> acc || checkAnt w h x) False as
    where checkAnt w h a = let Coord x y = loc a
                            in x >= 0 && x < w && y >= 0 && y < h


-- converts rule string to list of Rules
decodeRuleStr :: String -> [Rule]
decodeRuleStr = map (decodeRuleWord . (map toLower)) . words
    where decodeRuleWord w
            | isPrefixOf "turnl" w = TurnLeft
            | isPrefixOf "left"  w = TurnLeft
            | isPrefixOf "turnr" w = TurnRight
            | isPrefixOf "right" w = TurnRight
            | isPrefixOf "cont"  w = Continue
            | isPrefixOf "stra"  w = Continue
            | isPrefixOf "utur"  w = UTurn
            | otherwise            = error $ e w
          e x = "decodeRuleStr couldn't read word: " ++ x
    

-- converts ant string to list of ants
decodeAntStr :: String -> [Ant]
decodeAntStr = map (decodeAntWord . (split ',') . (map toLower)) . words
    where decodeAntWord (i:j:d:_) = newAnt (read i) (read j) (readDir d)
          decodeAntWord z         = error $ e (intercalate "," z)
          readDir ('l':_)         = Ant.Left
          readDir ('r':_)         = Ant.Right
          readDir ('u':_)         = Ant.Up
          readDir ('d':_)         = Ant.Down
          readDir w               = error $ e w
          e x = "decodeAntStr couldn't read word: " ++ x


-- splits a string by delimeter d
split :: Char -> String -> [String]
split d [] = [""]
split d (x:xs)
  | x == d    = "" : rest
  | otherwise = (x : head rest) : tail rest
      where rest = split d xs


-- makes a string of a board along with a summary of the arguments used to get
-- to it
prettyPrintBoard :: [Ant] -> Ruleset -> Int -> Int -> Integer -> Board -> String
prettyPrintBoard as (p, rs) h w n b =
    stringify b ++ "\nAnts: "    ++ (show as) ++
                   "\nRules: "   ++ (show rs) ++
                    "\nWrap? "   ++ (show p)  ++
                    "\nWidth: "  ++ (show w)  ++
                    "\nHeight: " ++ (show h)  ++
                    "\nIterations: " ++ (show n)


printBoards :: [Board] -> IO ()
printBoards bs = mapM_ printBoard bs 

printBoard :: Board -> IO ()
printBoard b = putStrLn $ '\n' : stringify b
