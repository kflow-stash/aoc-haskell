module Day2a (main) where

import System.IO 
import AOCFuncs
import qualified Data.List as DL
import Data.List.Split

data RPS = Rock | Paper | Scissors deriving (Eq, Show, Enum)
instance Ord RPS where
    compare Rock Paper = LT
    compare Paper Scissors = LT
    compare Scissors Rock = LT

type RPSRound = ((RPS, Int),(RPS, Int))

parseLine :: String -> RPSRound
parseLine line = (getRPS action1, getRPS action2)
    where (action1:action2:_) = words line

getRPS :: String -> (RPS,Int)
getRPS "A" = (Rock,1)
getRPS "B" = (Paper,2)
getRPS "C" = (Scissors,3)
getRPS "X" = (Rock,1)
getRPS "Y" = (Paper,2)
getRPS "Z" = (Scissors,3)

getRoundScore :: RPSRound -> Int
getRoundScore ((action1, _),(action2, action_score)) = win_score action1 action2 + action_score where 
    win_score a1 a2 
        | a1 == a2 = 3 
        | a1 > a2 = 0
        | a1 < a2 = 6

main = do  
    contents <- readFile "data/day2.txt"
    let game_in = map parseLine . lines $ contents

    let scores = sum (map getRoundScore game_in)
        
    print scores


