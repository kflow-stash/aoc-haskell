module Day2a (main) where

import System.IO 
import AOCFuncs

data RPS = Rock | Paper | Scissors deriving (Eq, Show, Enum, Ord)

compareRPS :: RPS -> RPS -> Ordering
compareRPS Rock Scissors = GT
compareRPS Scissors Rock = LT
compareRPS a b = a `compare` b

type RPSRound = (RPS, RPS)

parseLine :: String -> RPSRound
parseLine line = (getRPS action1, getRPS action2)
    where (action1:action2:_) = words line

getRPS :: String -> RPS
getRPS "A" = Rock
getRPS "B" = Paper
getRPS "C" = Scissors
getRPS "X" = Rock
getRPS "Y" = Paper
getRPS "Z" = Scissors

getRoundScore :: RPSRound -> Int
getRoundScore (action1,action2) = win_score action1 action2 + fromEnum action2 + 1 where 
    win_score a1 a2 
        | a1 == a2 = 3 
        | a1 `compareRPS` a2 == GT = 0
        | a1 `compareRPS` a2 == LT = 6

main = do  
    contents <- readFile "data/day2.txt"
    let game_in = map parseLine . lines $ contents

    let scores = map getRoundScore game_in
        sum_scores = sum scores
        
    print sum_scores


