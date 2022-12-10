module Day10 (main) where

import System.IO 
import AOCFuncs
import Data.List (tails,nub)

moveHead :: [[Int]] -> [Int] -> [[Int]]
moveHead [h0@[hy0,hx0], t0@[ty0,tx0]] [dy,dx] 
    | isAdjacent [hy1, hx1] t0 = [[hy1,hx1], t0]
    | otherwise = [[hy1,hx1], [ty0 + signum dy1,tx0 + signum dx1]]
        where   hy1 = hy0 + dy
                hx1 = hx0 + dx
                dy1 = hy1 - ty0
                dx1 = hx1 - tx0

isAdjacent :: [Int] -> [Int] -> Bool
isAdjacent [hy1,hx1] [ty0,tx0]
    | (abs (hx1 - tx0) > 1) || (abs (hy1 - ty0) > 1) = False
    | otherwise = True

parseIn :: [String] -> [[Int]]
parseIn [rel,steps] = replicate (readInt steps) (vectorize rel)
    where   vectorize "R" = [0,1]
            vectorize "L" = [0,-1]
            vectorize "U" = [-1,0]
            vectorize "D" = [1,0]

removeSucc :: [[Int]] -> [[Int]]
removeSucc [] = []
removeSucc [x] = [x]
removeSucc (x1:x2:xs)
            | x1 == x2 = removeSucc (x2:xs)
            | otherwise = x1 : removeSucc (x2:xs)

getTailMoves :: [[Int]] -> [[Int]]
getTailMoves [] = []
getTailMoves [x] = []
getTailMoves (x1:x2:xs) = [head x2 - head x1,last x2 - last x1]:getTailMoves (x2:xs)

applyMoves p0 moves = foldl (\acc x -> moveHead (head acc) x : acc) p0 vectors
    where vectors = getTailMoves $ removeSucc $ reverse $ map last moves


main = do  
    contents <- readFile "data/day10.txt"
    let input_ = map words $ lines contents



    print input_
