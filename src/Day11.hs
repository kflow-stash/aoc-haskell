module Day11 (main) where

import AOCFuncs
import Data.List (delete, intercalate, tails, zip, nub)
import Data.List.Split
import qualified Data.Map as M
import System.IO
import Data.Maybe

data Monkey = Monkey
  { operation_ :: Int -> Int,
    test_ :: Int,
    action1 :: Int,
    action2 :: Int
  }

f_ "+" = (+)
f_ "-" = (-)
f_ "*" = (*)

parseInput :: [String] -> (Int, Monkey, [Int])
parseInput [m_id, items', op', test', act1', act2'] = (id_, Monkey operation_ test_ action1 action2, items_)
  where
    id_ = readInt $ init m_id
    items_ = map (readInt . delete ',') $ tails (words items') !! 2
    op_arg = words op' !! 5
    test_ = readInt $ last $ words test'
    action1 = readInt $ last $ words act1'
    action2 = readInt $ last $ words act2'
    operation_ x
      | op_arg == "old" = f_ (words op' !! 4) x x
      | otherwise = f_ (words op' !! 4) (readInt op_arg) x

getOutput :: Monkey -> Int -> (Int,Int)
getOutput mp item_ = (to_monkey,adjusted_item)
  where
    adjusted_item = floor $ fromIntegral (operation_ mp item_) / 3
    to_monkey
      | adjusted_item `mod` test_ mp == 0 = action1 mp
      | otherwise = action2 mp

moveItems :: M.Map Int Monkey -> (M.Map Int [Int], M.Map Int [Int]) -> Int -> (M.Map Int [Int], M.Map Int [Int])
moveItems configs (bags, inspections) old_id = (new_bags, inspections2)
  where from_items = M.findWithDefault [] old_id bags
        from_config = fromJust $ M.lookup old_id configs
        to_items = map (getOutput from_config) from_items
        new_bags = foldl (\acc x -> addRemoveItem acc old_id x) bags to_items
        new_inspections = from_items ++ M.findWithDefault [] old_id inspections
        inspections2 = let f _ = Just new_inspections in M.alter f old_id inspections


addRemoveItem :: M.Map Int [Int] -> Int -> (Int,Int) -> M.Map Int [Int]
addRemoveItem  old_bags old_id (new_id,item_) = new_bags
  where to_bag = M.findWithDefault [] new_id old_bags
        new_to_bag  = to_bag ++ [item_]
        bags2 = let f _ = Just new_to_bag in M.alter f new_id old_bags
        from_bag = M.findWithDefault [] old_id bags2
        new_from_bag  = tail from_bag
        new_bags = let f _ = Just new_from_bag in M.alter f old_id bags2

completeRound :: M.Map Int Monkey -> ([M.Map Int [Int]],M.Map Int [Int]) -> Int -> ([M.Map Int [Int]],M.Map Int [Int])
completeRound configs (total_bags,total_inspections) n_monkeys = (new_bags : total_bags, new_inspections)
  where bags = head total_bags  
        (new_bags,new_inspections) = foldl (moveItems configs) (bags,total_inspections) [0..(n_monkeys-1)]

main = do
  contents <- readFile "data/day11.txt"
  let input_ = filter (/= []) $ map (filter (/= "") . lines) $ splitOn "Monkey " contents
      monkey_input = map parseInput input_
      initial_bags = [M.fromList [(id_,bag) | (id_, monkey, bag) <- monkey_input]]
      initial_configs = M.fromList [(id_,monkey) | (id_, monkey, bag) <- monkey_input]

      n_monkeys = length initial_configs
      rounds = foldl (\acc _ -> completeRound initial_configs acc n_monkeys) (initial_bags, M.empty) [0..19]
      monkey_inspections = map length [M.findWithDefault [] z (snd rounds)| z <- [0..(n_monkeys-1)]]

  print monkey_inspections

