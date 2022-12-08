module Day8 (main) where

import System.IO 
import AOCFuncs
import Data.List

visibleRow _ [] = []
visibleRow left_max (x:xs) = ((x > left_max) || all (<x) xs) : visibleRow new_max xs
    where new_max = max x left_max

scenicScoreR [_] = 0
scenicScoreR (x:xs) 
    | x <= head xs = 1
    | length xs == 1 = 1
    | otherwise = min (length (takeWhile (<x) xs) + 1) (length xs)

scenicScoreRow xs = reverse $ foldl (\acc x -> scenicScoreR x:acc) [] sights
    where sights = init $ tails xs

main = do  
    contents <- readFile "data/day8.txt"
    let input_ =  map (map (readInt . take 1) . init . tails) (lines contents)

        row_v = map (visibleRow (-1)) input_
        col_v = transpose $ map (visibleRow (-1)) $ transpose input_
        grid_v = [map fromEnum (zipWith (||) x y) | (x,y) <- zip row_v col_v]

        n_visible = sum $ map sum grid_v

        scenic_r = map scenicScoreRow input_
        scenic_l = map (reverse. scenicScoreRow . reverse) input_
        scenic_d = transpose $ map scenicScoreRow $ transpose input_
        scenic_u = transpose $ map (reverse . scenicScoreRow . reverse) $ transpose input_

        scenic = [zipWith4 (\r l d u -> r*l*d*u) r l d u | (r,l,d,u) <- zip4 scenic_r scenic_l scenic_d scenic_u]
        max_scenic = maximum $ map maximum scenic
        
    print $ "part 1 answer: " ++ show n_visible
    print $ "part 2 answer: " ++ show max_scenic



    
    





        