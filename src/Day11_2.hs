module Day11_2 (main) where

import AOCFuncs
import Data.List (delete, intercalate, tails, zip, nub, sortBy)
import Data.List.Split
import qualified Data.Map as M
import System.IO
import Data.Maybe
import Data.Function

data Monkey = Monkey
  { operation_ :: (Int -> Int -> Int) -> Int -> String -> Int,
    op_arg :: String,
    operator :: Int -> Int -> Int,
    test_ :: Int,
    action1 :: Int,
    action2 :: Int
  }

f_ "+" = (+)
f_ "-" = (-)
f_ "*" = (*)

computeMods :: M.Map Int Monkey -> Monkey -> [Int] -> [Int]
computeMods configs monkey_ xs = map (\(x,(_,config)) -> operation_ config (operator monkey_) x (op_arg monkey_)) $ zip xs (M.toList configs)

computeInitialMods :: M.Map Int Monkey -> Monkey -> Int -> [Int]  
computeInitialMods configs monkey_ x = map (\(y,(id,config)) -> mod y (test_ config))  $ zip (repeat x) (M.toList configs)

parseInput :: [String] -> (Int, Monkey, [Int])
parseInput [m_id, items', op', test', act1', act2'] = (id_, Monkey operation_ op_arg operator test_ action1 action2, items_)
  where
    id_ = readInt $ init m_id
    items_ = map (readInt . delete ',') $ tails (words items') !! 2
    op_arg = words op' !! 5
    test_ = readInt $ last $ words test'
    action1 = readInt $ last $ words act1'
    action2 = readInt $ last $ words act2'
    operator = f_ (words op' !! 4)
    operation_ source_op x_mod x_arg
      | x_arg == "old" = source_op x_mod x_mod `mod` test_
      | otherwise = source_op (readInt x_arg `mod` test_) x_mod `mod` test_
      

getOutput :: Int -> Monkey -> [Int] -> (Int,[Int])
getOutput monkey_id mp item_ = (to_monkey,item_)
  where
    mod_val = item_ !! monkey_id
    to_monkey = if mod_val == 0 then action1 mp else action2 mp 


moveItems :: M.Map Int Monkey -> (M.Map Int [[Int]], M.Map Int Int) -> Int -> (M.Map Int [[Int]], M.Map Int Int)
moveItems configs (bags, inspections) old_id = (new_bags, inspections2)
  where from_items = fromJust $ M.lookup old_id bags
        from_config = fromJust $ M.lookup old_id configs
        from_mods = map (computeMods configs from_config) from_items
        to_items = map (getOutput old_id from_config) from_mods
        new_bags = foldl (\acc x -> addRemoveItem acc old_id x) bags to_items
        new_inspections = length from_items + M.findWithDefault 0 old_id inspections
        inspections2 = let f _ = Just new_inspections in M.alter f old_id inspections


addRemoveItem :: M.Map Int [[Int]] -> Int -> (Int,[Int]) -> M.Map Int [[Int]]
addRemoveItem  old_bags old_id (new_id,item_) = new_bags
  where to_bag = M.findWithDefault [] new_id old_bags
        new_to_bag  = to_bag ++ [item_]
        bags2 = let f _ = Just new_to_bag in M.alter f new_id old_bags
        from_bag = M.findWithDefault [] old_id bags2
        new_from_bag  = tail from_bag
        new_bags = let f _ = Just new_from_bag in M.alter f old_id bags2

completeRound :: M.Map Int Monkey -> ([M.Map Int [[Int]]],M.Map Int Int) -> Int -> ([M.Map Int [[Int]]],M.Map Int Int)
completeRound configs (total_bags,total_inspections) n_monkeys = (new_bags : total_bags, new_inspections)
  where bags = head total_bags  
        (new_bags,new_inspections) = foldl (moveItems configs) (bags,total_inspections) [0..(n_monkeys-1)]

main = do
  contents <- readFile "data/day11.txt"
  let input_ = filter (/= []) $ map (filter (/= "") . lines) $ splitOn "Monkey " contents
      monkey_input = map parseInput input_
      initial_bags = [M.fromList [(id_,bag) | (id_, monkey, bag) <- monkey_input]]
      initial_configs = M.fromList [(id_,monkey) | (id_, monkey, bag) <- monkey_input]

      initial_mod_bags = M.fromList [(id_,map (computeInitialMods initial_configs monkey) bag) | (id_, monkey, bag) <- monkey_input]

      n_monkeys = length initial_configs

      rounds = foldl (\acc _ -> completeRound initial_configs acc n_monkeys) ([initial_mod_bags], M.empty) [0..9999]

      monkey_inspections = sortBy (flip compare `on` snd) (M.toList $ snd rounds)

  print monkey_inspections

