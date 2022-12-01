module Day1 (main) where

import System.IO 
import AOCFuncs
import qualified Data.List as DL
import Data.List.Split

main = do  
    contents <- readFile "data/day1.txt"
    let elf_loads = map (map readInt . lines) (splitOn "\n\n" contents)
        elf_sums = DL.sort $ map sum elf_loads

    --let lines_ = concat $ splitElfs $ DL.lines contents
    --    lines_str = map (wordsWhen (== ',')) (wordsWhen (==';') lines_)
    --    lines_int = map (map readInt) lines_str
    --    elf_sums = DL.sort $ map sum lines_int

    print $ "largest load: " ++ show (last elf_sums)

    print $ "top 3 loads: " ++ show (take 3 (reverse elf_sums))

    print $ "sum of top 3 loads: " ++ show (sum (take 3 (reverse elf_sums)))


splitElfs :: [String] -> [String]
splitElfs [] = []
splitElfs (x : "" : xs) = x : ";" : splitElfs xs
splitElfs (x:xs) = (x ++ ",") : splitElfs xs
