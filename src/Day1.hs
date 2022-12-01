module Day1 (main) where

import System.IO 
import AOCFuncs
import Data.List
import Data.List.Utils

main = do  
    contents <- readFile "data/day1.txt"
    let lines_ = concat $ splitElfs $ lines contents
        lines_str = map (wordsWhen (== ',')) (wordsWhen (==';') lines_)
        lines_int = map (map readInt) lines_str

        elf_loads = sort $ map sum lines_int

    print $ "largest load: " ++ show (last elf_loads)

    print $ "top 3 loads: " ++ show (take 3 (DL.reverse elf_loads))

    print $ "sum of top 3 loads: " ++ show (sum (take 3 (DL.reverse elf_loads)))


splitElfs :: [String] -> [String]
splitElfs [] = []
splitElfs (x : "" : xs) = x : ";" : splitElfs xs
splitElfs (x:xs) = (x ++ ",") : splitElfs xs
