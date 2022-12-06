module Day6 (main) where

import System.IO 
import AOCFuncs
import Data.List
import qualified Data.Set as Set

main = do  
    contents <- readFile "data/day6.txt"
    let 
        part1 = length (takeWhile (/=4) $ map (length . Set.fromList . take 4) $ tails contents) + 4
        part2 = length (takeWhile (/=14) $ map (length . Set.fromList . take 14) $ tails contents) + 14
        
    print part1
    print part2

        