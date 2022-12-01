module Day1 where

import System.IO 

main = do  
    contents <- readFile "test.txt"
    let input_nums = map readInt . words $ contents
    print input_nums

readInt :: String -> Int
readInt = read