module Day3 (main) where

import System.IO 
import AOCFuncs
import Data.Maybe
import Data.List

getErrors :: String -> Char
getErrors line = head $ intersect sack1 sack2
            where   n_items = length line `div` 2
                    sack1 = take n_items line
                    sack2 = take n_items (reverse line)

getBadge :: [String] -> Char
getBadge [sack1,sack2,sack3] = head $ intersect (sack1 `intersect` sack2) sack3

scoreLookup :: Char -> Int
scoreLookup x = fromMaybe 0 (lookup x scores)
            where scores = zip (['a'..'z'] ++ ['A'..'Z']) [1..]

takeThree :: [String] -> [[String]]
takeThree [] = []
takeThree (x1:x2:x3:xs) = [x1,x2,x3] : takeThree xs

main = do  
    contents <- readFile "data/day3.txt"
    let rucksacks = lines contents
        score_total = foldl (\acc x -> scoreLookup x + acc) 0 (map getErrors rucksacks)
        badge_total = foldl (\acc x -> scoreLookup x + acc) 0 (map getBadge $ takeThree rucksacks)

    print $ "part 1 answer: " ++ show score_total
    print $ "part 2 answer: " ++ show badge_total
        