module Day5 (main) where

import System.IO 
import AOCFuncs
import Data.List

getLocations :: String -> [String]
getLocations [] = []
getLocations (x1:x2:x3:' ':xs) = [x2] : getLocations xs
getLocations (x1:x2:x3:xs) = [x2] : getLocations xs

removeCrates :: [Int] -> [[String]] -> [[String]]
removeCrates [n_crates, fromcrate, tocrate] locs =  let (xs1, x:xs2) = splitAt (fromcrate-1) locs
                                                    in xs1 ++ (tails x !! n_crates):xs2

addCrates :: [Int] -> [[String]] -> [[String]]
addCrates [n_crates, fromcrate, tocrate] locs = let     crates = take n_crates (locs !! (fromcrate - 1))
                                                        (xs1, x:xs2) = splitAt (tocrate-1) locs
                                                in      xs1 ++ (reverse crates ++ x):xs2

moveCrates :: [Int] -> [[String]] -> [[String]]
moveCrates [n_crates, a, b] locs = zipWith addRemoveCrates locs [1 .. ]
            where   crates = take n_crates (locs !! (a - 1))
                    addRemoveCrates xs crate_id     | a == crate_id  = tails xs !! n_crates
                                                    | b == crate_id  = reverse crates ++ xs
                                                    | otherwise      = xs

moveCratesPart2 :: [Int] -> [[String]] -> [[String]]
moveCratesPart2 [n_crates, a, b] locs = zipWith addRemoveCrates locs [1 .. ]
            where   crates = take n_crates (locs !! (a - 1))
                    addRemoveCrates xs crate_id     | a == crate_id  = tails xs !! n_crates
                                                    | b == crate_id  = crates ++ xs
                                                    | otherwise      = xs


main = do  
    contents <- readFile "data/day5.txt"
    let (locations, instructions) = break (=="") $ lines contents
        stacks = map (filter (/= " ")) $ transpose $ init $ fmap getLocations locations
        instructs = map ((\x -> [readInt $ x !! 1, readInt $ x !! 3, readInt $ x !! 5]) . wordsWhen (==' ')) (tail instructions)

        final_crates1 = foldl (\acc x -> removeCrates x (addCrates x acc)) stacks instructs
        top_crates1 = concatMap head final_crates1

        final_crates2 = foldl (flip moveCratesPart2) stacks instructs
        top_crates2 = concatMap head final_crates2


    print instructs
    print stacks

    print $ "part 1 answer: " ++ top_crates1
    print $ "part 2 answer: " ++ top_crates2
        