module Rule
    ( Rule(..)
    , Ruleset(..)
    , turn
    ) where

import qualified Ant as A

data Rule = TurnLeft | TurnRight | Continue | UTurn deriving (Show)
type Ruleset = (Bool, [Rule]) -- wrap y/n and rule list


turn :: A.Dir -> Rule -> A.Dir
turn d Continue  = d
turn d TurnLeft  = A.prevDir d
turn d TurnRight = A.nextDir d
turn d UTurn     = A.nextDir (A.nextDir d)

