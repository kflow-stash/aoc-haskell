module Day1 (main) where

import System.IO 
import AOCFuncs
import qualified Data.List as DL
import Data.List.Split

main = do  
    contents <- readFile "data/day1.txt"
    let elf_loads = map (map readInt . lines) (splitOn "\n\n" contents)
        elf_sums = DL.sortBy (flip compare) $ map sum elf_loads

    print $ "largest load: " ++ show (head elf_sums)

    print $ "top 3 loads: " ++ show (take 3 elf_sums)

    print $ "sum of top 3 loads: " ++ show (sum (take 3 elf_sums))

