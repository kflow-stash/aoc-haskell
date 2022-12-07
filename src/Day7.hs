module Day7 (main) where

import System.IO 
import AOCFuncs
import Data.List
import qualified Data.Set as Set
import Data.List.Split
import Data.Maybe
import qualified Data.Map as Map
import Data.Function

data DirContents = DirContents {fileSize :: Int, subdirs :: [String]} deriving (Show, Eq)

getDirMap :: [[String]] -> [(String, DirContents)]
getDirMap [_] = [("",DirContents 0 [])]
getDirMap (x_pre:[".."]:xs) = getDirMap ([moveup_path]:xs) where moveup_path = intercalate "\\" (init (wordsWhen (=='\\') (head x_pre)))
getDirMap (x_pre:x:xs) = getDirElem x_pre x : getDirMap ([dir_path]:xs)
    where 
        dir_path = case x_pre of    [] -> head x 
                                    _ -> head x_pre ++ "\\" ++ head x
        

getDirElem :: [String] -> [String] -> (String, DirContents)
getDirElem x_pre x = (dir_path, DirContents (getFileSizes x) (getSubDirs dir_path x))
    where dir_path = case x_pre of  [] -> head x 
                                    _ -> head x_pre ++ "\\" ++ head x

getSubDirs :: String -> [String] -> [String]
getSubDirs prefix_ x = map (((prefix_ ++ "\\") ++) . last . words) (filter (\x -> "dir " `isPrefixOf` x) x)
    
getTotalSize :: (String,DirContents) -> Map.Map String Int -> Map.Map String Int
getTotalSize (dir_name, dir_contents) totals = Map.insert dir_name new_entry totals
    where new_entry = fileSize dir_contents + foldl (\acc x -> acc + fromMaybe 0 (Map.lookup x totals)) 0 (subdirs dir_contents) 

getFileSizes :: [String] -> Int
getFileSizes x = sum $ map getFileSize (tail x)
    where getFileSize x
            | "dir " `isPrefixOf` x = 0
            | "$ " `isPrefixOf` x = 0
            | otherwise = readInt $ head $ words x

main = do  
    contents <- readFile "data/day7.txt"
    let input_ = map lines (splitOn "$ cd " contents)
        directory_map =  filter (/=("", DirContents 0 [])) (getDirMap input_)
        sorted_directs = sortBy (flip compare `on` length . splitOn "\\" . fst) directory_map
        total_sizes = Map.toList (foldl (flip getTotalSize) Map.empty sorted_directs )

        part1 = sum $ map snd (filter (\(dir,size_) -> size_<=100000) total_sizes)

        total_space = 40000000 :: Int
        total_needed = maximum (map snd total_sizes) - total_space
        part2 = snd $ minimumBy (compare `on` snd) (filter (\(dir,size_) -> size_>=total_needed) total_sizes)

    print part1
    print part2




        