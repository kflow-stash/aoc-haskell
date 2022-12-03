module Day2 (main) where

import System.IO 
import AOCFuncs

data RPS = Rock | Paper | Scissors deriving (Eq, Show, Enum, Ord)
type RPSRound = (RPS, RPS)

compareRPS :: RPS -> RPS -> Ordering
compareRPS Rock Scissors = GT
compareRPS Scissors Rock = LT
compareRPS a b = a `compare` b

succ' :: RPS -> RPS
succ' Scissors = Rock
succ' rps_ = succ rps_

pred' :: RPS -> RPS
pred' Rock = Scissors
pred' rps_ = pred rps_

parseAction :: String -> RPSRound
parseAction line = (getRPS action1, getRPS action2)
    where (action1:action2:_) = words line

parseResponse :: String -> RPSRound
parseResponse line = responseAction (getRPS action) result
    where (action:result:_) = words line

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

responseAction :: RPS -> String -> RPSRound
responseAction action result = (action, result_action)
    where result_action
            | result == "X" = pred' action
            | result == "Y" = action
            | result == "Z" = succ' action

main = do  
    contents <- readFile "data/day2.txt"
    let game_in = lines contents
        score_round1 = sum (map (getRoundScore . parseAction) game_in)
        score_round2 = sum (map (getRoundScore . parseResponse) game_in)
        
    print $ "part 1 answer: " ++ show score_round1
    print $ "part 2 answer: " ++ show score_round2


