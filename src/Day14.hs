module Day14 (main) where

import AOCFuncs
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO
import qualified Data.HashSet as DS

interpCoords c1@[y1, x1] c2@[y2, x2] 
  | c1 == c2 = [c2]
  | otherwise = c1 : interpCoords c3 c2 
    where c3 = [signum (y2 - y1) + y1, signum (x2 - x1) + x1]

getOccupied :: [[[Int]]] -> DS.HashSet [Int]
getOccupied coords = foldl DS.union DS.empty points
  where 
    windows x = map (take 2) $ tails x
    pairs = map (filter (\x -> length x == 2) . windows) coords
    interp_coords x = DS.fromList $ concatMap (\[x,y] -> interpCoords x y) x
    points = map interp_coords pairs

sandFall :: [Int] -> DS.HashSet [Int] -> Int -> DS.HashSet [Int]
sandFall z0 occ max_y
  | y1 > max_y = occ
  | y1 == 0 = occ2
  | otherwise = sandFall z0 occ2 max_y
    where [x1,y1] = fallDown z0 occ max_y
          occ2 = DS.insert [x1,y1] occ

fallDown :: [Int] -> DS.HashSet [Int] -> Int -> [Int]
fallDown [x,y] occ max_y
  | y > max_y = [x,y]
  | not (DS.member [x,y+1] occ) = fallDown [x,y+1] occ max_y
  | not (DS.member [x-1,y+1] occ) = fallDown [x-1,y+1] occ max_y
  | not (DS.member [x+1,y+1] occ) = fallDown [x+1,y+1] occ max_y
  | otherwise = [x,y]

main = do
  contents <- readFile "data/day14.txt"
  let input_coords = map (map (map readInt . wordsWhen (== ',')) . splitOn " -> ") (lines contents)
      occupied = getOccupied input_coords

      max_y = maximum [y | [x,y] <- DS.toList occupied]

      occupied_sand = sandFall [500,0] occupied max_y
      n_grains = length occupied_sand - length occupied

      x_coords = [x | [x,y] <- DS.toList occupied]
      minx = minimum x_coords
      maxx = maximum x_coords
      occupied_part2 = foldl (\acc (x,y) -> DS.insert [x,y] acc) occupied (zip [(minx-150)..(maxx+150)] (repeat (max_y + 2)))

      occupied_sand2 = sandFall [500,0] occupied_part2 (max_y+2)
      n_grains2 = length occupied_sand2 - length occupied_part2
      

  print n_grains

  print n_grains2

