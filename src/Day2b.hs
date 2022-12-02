module Day2b (main) where

import System.IO 
import AOCFuncs

data RPS = Rock | Paper | Scissors deriving (Eq, Show, Enum)
instance Ord RPS where
    compare Rock Paper = LT
    compare Paper Scissors = LT
    compare Scissors Rock = LT

type RPSRound = ((RPS, Int),(RPS, Int))

succ' :: RPS -> RPS
succ' Scissors = Rock
succ' rps_ = succ rps_

pred' :: RPS -> RPS
pred' Rock = Scissors
pred' rps_ = pred rps_

responseAction :: RPS -> String -> RPSRound
responseAction action result = ((action, getRPSVal action), (result_action, getRPSVal result_action))
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

getRPSVal :: RPS -> Int
getRPSVal action
    | action == Rock = 1
    | action == Paper = 2
    | action == Scissors = 3

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


