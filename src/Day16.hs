module Day16 (main) where

import AOCFuncs
import Data.Char
import Data.List
import Data.List.Split
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.Maybe
import System.IO
import qualified Data.HashSet as DS
import Data.Function

data BFS = BFS
  {
    valveQueue :: MinPrioHeap Int String,
    visited :: HashSet String,
    distanceMap :: HashMap String Int,
    valveMap :: HashMap String [String]
  }

initialDistances src vM iV = removedMap
  where 
    map_ = HM.delete src (findDistances (BFS (H.fromList [(0, src)]) HS.empty (HM.singleton src 0) vM))
    removedMap = HM.toList $ foldl (flip HM.delete) map_ iV

findDistances :: BFS -> HashMap String Int
findDistances bfs@(BFS vQ0 vS0 dM0 vM0) = case H.view vQ0 of
  Nothing -> dM0
  Just ((dist, valve), vQ1) 
    | HS.member valve vS0 -> findDistances (bfs {valveQueue=vQ1})
    | otherwise -> 
        let vS1 = HS.insert valve vS0
            neighbors = fromMaybe [] (HM.lookup valve vM0)
            unvisited = filter (\v -> not (HS.member v vS1)) neighbors
        in  findDistances $ foldl 
                            (foldNeighbors (dist+1)) 
                            (BFS vQ0 vS1 dM0 vM0) 
                            unvisited

foldNeighbors dist bfs@(BFS vQ0 vS0 dM0 vM0) valve = if not exists' then BFS (H.insert (dist,valve) vQ0) vS0 (HM.insert valve dist dM0) vM0 else bfs
  where exists' = HM.member valve dM0 && (fromJust (HM.lookup valve dM0) < dist)

data PathObj = PathObj 
  {
    openedSet :: HashSet String,
    openPath :: String,
    openTime :: [Int],
    openFlow :: [Int],
    vD :: [Int],
    minutesRemaining :: Int,
    flowRate :: Int,
    currentValve :: String,
    dMap :: HashMap String [(String,Int)],
    frMap :: HashMap String Int
  } deriving (Show)

bySecond (_, a, _, _,_ ) (_,  b, _, _,_ ) 
  | a<b = LT
  | a>b = GT
  | a==b = EQ   

byFirst :: (String, a,b,c,d) -> (String, a,b,c,d) -> Ordering
byFirst (x, _, _, _,_ ) (y,  _, _, _,_ ) 
  | x< y= LT
  | x>y= GT
  | x==y= EQ     

byThird (_,_,a) (_,_,b)
  | a<b = LT
  | a>b = GT
  | a==b = EQ
  

brute :: PathObj -> [(String, Int, [Int], [Int],[Int])]
brute p0@(PathObj vOpen vPath vTime vFR vD tr fr src vM frM) 
  | tr < 2 = [(vPath, fr, vTime, vFR, vD)]
  | otherwise = case length relValves of
                    0 -> [(vPath, fr, vTime, vFR, vD)]
                    _ -> (vPath, fr, vTime, vFR, vD) : foldl (\acc x -> brute x ++ acc) [] valvePaths
      where valves = fromJust $ HM.lookup src vM
            relValves = filter (\(v,dist) -> (dist < (tr - 1)) && not (HS.member v vOpen)) valves
            rateValves = foldl (\acc (v,dist) -> (v,dist,fromJust $ HM.lookup v frM):acc) [] relValves
            valvePaths = map (takeStep p0) rateValves

takeStep :: PathObj -> (String,Int,Int) ->  PathObj
takeStep p@(PathObj vOpen vPath vTime vFR vD tr fr src vM frM) (eValve, dist, flowR) = PathObj vOpen' vPath' vTime' vFR' vD' tr' fr' eValve vM frM  
  where vOpen' = HS.insert src vOpen
        vPath' = vPath ++ eValve
        vD' = vD ++ [dist]
        tr' = tr - dist - 1
        vTime' = vTime ++ [tr']
        fr' = fr + (tr' * flowR)
        vFR' = vFR ++ [tr' * flowR]

findDisjoint :: [(HashSet String,Int)] -> Int
findDisjoint [] = 0
findDisjoint (x:xs) = max new_max (findDisjoint xs)
  where 
    disjoint_paths = filter (\((v1,_), (v2, _)) -> disjoint' v1 v2) ( map ((,) x) xs)
    new_max = if null disjoint_paths then 0 else maximum [z1 + z2 | ((_,z1), (_, z2)) <- disjoint_paths]


disjoint' :: HashSet String -> HashSet String -> Bool
disjoint' x1 x2 = null $ HS.intersection x1 x2

main = do
  contents <- readFile "data/day16.txt"
  let input_ = map (map (wordsWhen (==',')) . wordsWhen (==';')) (lines contents)
      edges = map (map (reverse . take 2 . reverse) . last) input_
      valves = concatMap (map (take 2 . drop 6) . head ) input_
      flow_rates = concatMap (map (readInt . last . wordsWhen (=='=')) . head ) input_

      valveMap = HM.fromList $ zip valves edges
      rateMap = HM.fromList $ zip valves flow_rates

      activeValves = sortBy (compare `on` fst) [(x,y) | (x,y)<- HM.toList rateMap, y>0]
      inertValves = [x | (x,y) <- zip valves flow_rates, y == 0]
      distanceMap = foldl (\acc src -> HM.insert src (initialDistances src valveMap inertValves) acc) HM.empty valves

      part1 = sortBy (flip bySecond) $ brute (PathObj HS.empty [] [] [] [] 30 0 "AA" distanceMap rateMap)

      part2_all = sortBy (flip byFirst) $ brute (PathObj HS.empty [] [] [] [] 26 0 "AA" distanceMap rateMap)
      compare_paths = HS.fromList [(HS.fromList $ chunksOf 2 x,y) | (x,y,_,_,_) <- part2_all]
      part2 =  findDisjoint $ HS.toList compare_paths


  print $ head part1
  print part2



