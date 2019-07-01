module Rule
    ( Instruction(..)
    , Rule(..)
    , turn
    ) where

import qualified Ant as A

data Instruction = TurnLeft | TurnRight | Continue | UTurn deriving (Show)
data Rule = Rule { inst :: Instruction
                 , color :: String -- change this once you get GUI working
                 } deriving (Show)


turn :: A.Dir -> Instruction -> A.Dir
turn d Continue  = d
turn d TurnLeft  = A.leftOf d
turn d TurnRight = A.rightOf d
turn d UTurn     = A.rightOf (A.rightOf d)

