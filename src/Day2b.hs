module Day2b (main) where

import System.IO 
import AOCFuncs

data RPS = Rock | Paper | Scissors deriving (Eq, Show, Enum, Ord)
compareRPS :: RPS -> RPS -> Ordering
compareRPS Rock Scissors = GT
compareRPS Scissors Rock = LT
compareRPS a b = a `compare` b

type RPSRound = (RPS, RPS)

succ' :: RPS -> RPS
succ' Scissors = Rock
succ' rps_ = succ rps_

pred' :: RPS -> RPS
pred' Rock = Scissors
pred' rps_ = pred rps_

responseAction :: RPS -> String -> RPSRound
responseAction action result = (action, result_action)
    where result_action
            | result == "X" = pred' action
            | result == "Y" = action
            | result == "Z" = succ' action

parseLine :: String -> RPSRound
parseLine line = responseAction (getRPS action) result
    where (action:result:_) = words line

getRPS :: String -> RPS
getRPS "A" = Rock
getRPS "B" = Paper
getRPS "C" = Scissors

getRoundScore :: RPSRound -> Int
getRoundScore (action1, action2) = win_score action1 action2 + fromEnum action2 + 1 where 
    win_score a1 a2 
        | a1 == a2 = 3 
        | a1 `compareRPS` a2 == GT = 0
        | a1 `compareRPS` a2 == LT = 6

main = do  
    contents <- readFile "data/day2.txt"
    let game_in = map parseLine . lines $ contents

    let scores = sum (map getRoundScore game_in)
        
    print scores


