module Day15 (main) where

import AOCFuncs
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO
import qualified Data.HashSet as DS
import Data.Function

distanceCalc :: (Int,Int) -> (Int,Int) -> Int
distanceCalc (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

scannerPoints :: ((Int,Int),Int) -> Int -> (Int,Int)
scannerPoints ((x,y),d) row_ = if dy < 0 then (0,0) else (x - dy, x + dy)
  where 
    dy = d - abs (y-row_)

getMissing [] _ = []
getMissing (x@(x1,x2):xs) t@(total1, total2) 
  | x2 < total2 = getMissing xs t
  | x1 > total2 + 1 = [total2 + 1] : getMissing xs (total1, x2)
  | otherwise = getMissing xs (total1, x2)

getAllMissing2 row_ distances = getMissing rel_scans (0,0)
  where
      scan_ranges = sortBy (compare `on` fst) $ filter (/= (0,0)) $ map (`scannerPoints` row_) distances
      rel_scans = filter (\(x1,x2) -> (x2 >= 0) && (x1 <= 4000000) ) scan_ranges

getAllMissing1 row_ distances = max_x - min_x + 1 - length missed
  where
      scan_ranges = sortBy (compare `on` fst) $ filter (/= (0,0)) $ map (`scannerPoints` row_) distances
      min_x = minimum $ map fst scan_ranges
      max_x = maximum $ map snd scan_ranges
      missed = getMissing scan_ranges (min_x,min_x)

main = do
  contents <- readFile "data/day15.txt"
  let input_ = map (map (wordsWhen (==',')) . wordsWhen (==':')) (lines contents)
      input2 = map (map (map (readInt . last . wordsWhen (=='=')))) input_

      distances = map (\[[x1,y1],[x2,y2]] -> ((x1,y1), distanceCalc (x1,y1) (x2,y2))) input2

      row_ = 2000000
      part1_scanned = getAllMissing1 row_ distances
      rel_beacons = DS.filter (\[x,y] -> y == row_) $ DS.fromList $ map last input2
      part1 = part1_scanned - length rel_beacons

      beacon_loc = head $ foldl (\acc x -> let missing = getAllMissing2 x distances in if null missing then acc else (head $ head missing,x):acc ) [] [3000000..4000000]


  print part1
  print $ fst beacon_loc * 4000000 + snd beacon_loc





