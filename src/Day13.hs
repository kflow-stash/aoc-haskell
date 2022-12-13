module Day13 (main) where

import AOCFuncs
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

isList = (== '[')

getNum = break (\x -> (x == ']') || (x == ','))

dropLeading = dropWhile (\x -> (x == ']') || (x == ','))

dropLeadingComma = dropWhile (== ',')

isEnd x = (== ']') $ head x

insertList xs = "[" ++ x1 ++ "]" ++ x2
  where (x1,x2) = break (==',') xs

decodeElem :: [String] -> Bool
decodeElem [[], []] = False
decodeElem [[], xrs] = True
decodeElem [xls, []] = False
decodeElem [']' : xls, ']' : xrs] = decodeElem [dropLeadingComma xls, dropLeadingComma xrs]
decodeElem [']' : xls, xrs] = True
decodeElem [xls, ']' : xrs] = False
decodeElem [x1@(xl : xls), x2@(xr : xrs)]
  | isList xl && isList xr = decodeElem [xls, xrs]
  | isList xl && not (isList xr) = decodeElem [xl : xls, insertList x2]
  | not (isList xl) && isList xr = decodeElem [insertList x1, xr : xrs]
  | let (l_num, xls') = getNum (xl : xls)
        (r_num, xrs') = getNum (xr : xrs)
        xls'' = dropLeading xls'
        xrs'' = dropLeading xrs'
        compareNums l r
          | readInt l < readInt r = True
          | readInt l > readInt r = False
          | readInt l == readInt r = case (isEnd xls', isEnd xrs') of
              (True, False) -> True
              (False, True) -> False
              _ -> decodeElem [xls'', xrs''] =
      compareNums l_num r_num

swapTill x [] = [x]
swapTill x (y : xs) = min' x y : swapTill (max' x y) xs

min' x1 x2
  | x1 == x2 = x1
  | decodeElem [x1, x2] = x1
  | otherwise = x2

max' x1 x2
  | x1 == x2 = x1
  | decodeElem [x1, x2] = x2
  | otherwise = x1


main = do
  contents <- readFile "data/day13.txt"
  let input_ = map lines $ splitOn "\n\n" contents

      return_ = zip [1 ..] (map decodeElem input_)
      trues = map fst $ filter snd return_
      part1 = sum trues

      flat_input = concat input_ ++ ["[[2]]", "[[6]]"]
      bubbleSort = foldl (flip swapTill) [] flat_input
      id1 = fromJust (elemIndex "[[2]]" bubbleSort) + 1
      id2 = fromJust (elemIndex "[[6]]" bubbleSort) + 1

      part2 = id1 * id2

  print part1

  print part2