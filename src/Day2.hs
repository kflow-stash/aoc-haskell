module Day2 (main) where

import System.IO 
import AOCFuncs
import qualified Data.List as DL
import Data.List.Split

main = do  
    contents <- readFile "data/day2.txt"
    let elf_loads = map (map readInt . lines) (splitOn "\n\n" contents)
        elf_sums = DL.sort $ map sum elf_loads
        
    print $ "largest load: " ++ show (last elf_loads)

    print $ "top 3 loads: " ++ show (take 3 (reverse elf_loads))

    print $ "sum of top 3 loads: " ++ show (sum (take 3 (reverse elf_loads)))


splitElfs :: [String] -> [String]
splitElfs [] = []
splitElfs (x : "" : xs) = x : ";" : splitElfs xs
splitElfs (x:xs) = (x ++ ",") : splitElfs xs
