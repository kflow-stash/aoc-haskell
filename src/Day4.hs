module Day4 (main) where

import System.IO 
import AOCFuncs
import Data.List

computeSubsets :: [[Int]] -> Int
computeSubsets [x,y] = fromEnum (((head x >= head y) && (tail x <= tail y)) || ((head x <= head y) && (tail x >= tail y)))

computeOverlaps :: [[Int]] -> Int
computeOverlaps [[x1,x2],[y1,y2]] = fromEnum (not (null ([x1..x2] `intersect` [y1..y2])))

main = do  
    contents <- readFile "data/day4.txt"
    let sections = map ((\[x,y] -> [map readInt (wordsWhen (=='-') x), map readInt (wordsWhen (=='-') y)] ) . wordsWhen (==',')) (lines contents)
        n_subsets = sum $ map computeSubsets sections
        n_overlaps = sum $ map computeOverlaps sections

    print $ "part 1 answer: " ++ show n_subsets
    print $ "part 2 answer: " ++ show n_overlaps
        