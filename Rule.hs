module Rule
    ( Instruction(..)
    , Rule(..)
    , Ruleset(..)
    , turn
    ) where

import qualified Ant as A

data Instruction = TurnLeft | TurnRight | Continue | UTurn deriving (Show)
data Rule = Rule { inst :: Instruction
                 , color :: String -- change this once you get GUI working
                 } deriving (Show)
type Ruleset = (Bool, [Rule]) -- wrap y/n and rule list


turn :: A.Dir -> Instruction -> A.Dir
turn d Continue  = d
turn d TurnLeft  = A.prevDir d
turn d TurnRight = A.nextDir d
turn d UTurn     = A.nextDir (A.nextDir d)

