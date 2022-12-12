module Day12 (main) where

import AOCFuncs
import Data.Char
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.List (delete, elemIndex, elemIndices, intercalate, nub, tails, zip)
import Data.List.Split
import Data.Maybe (fromJust, fromMaybe)
import System.IO

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist x = False
  Dist x <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Hashable k) => HashMap k (Distance d,(Int,Int)) -> k -> Distance d
(!??) distanceMap key = fst $ fromMaybe (Infinity, (-1,-1)) (HM.lookup key distanceMap)

newtype Graph = Graph
  {edges :: HashMap (Int, Int) [(String, Int)]}

data DijkstraState = DijkstraState
  { visitedSet :: HashSet (Int, Int),
    distanceMap :: HashMap (Int, Int) (Distance Int,(Int,Int)),
    nodeQueue :: MinPrioHeap (Distance Int) (Int, Int),
    grid :: HashMap (Int, Int) Int,
    bnds :: (Int, Int, Int, Int),
    src :: (Int, Int),
    dest :: (Int, Int)
  }

neighboring = [(0, 1), (1, 0), (0, -1), (-1, 0)]

getNeighbors :: (Int, Int) -> DijkstraState -> [((Int, Int), Int)]
getNeighbors src_ ds@(DijkstraState _ _ _ grid_ (lowB, highB, leftB, rightB) _ _) = map (\node -> (node, HM.findWithDefault 0 node grid_)) inNeighbors
  where
    inBounds (y, x)
      | y < lowB = False
      | y > highB = False
      | x < leftB = False
      | x > rightB = False
      | otherwise = True
    inNeighbors = filter inBounds $ map (\(dy, dx) -> (dy + fst src_, dx + snd src_)) neighboring

processQueue :: DijkstraState -> HashMap (Int, Int) (Distance Int,(Int,Int))
processQueue ds@(DijkstraState v0 d0 q0 grid_ bnd src dest) = case H.view q0 of
  Nothing -> d0
  Just ((minDist, node), q1)
    | node == dest -> d0
    | HS.member node v0 -> processQueue (ds {nodeQueue = q1})
    | otherwise ->
        let v1 = HS.insert node v0
            allNeighbors = getNeighbors node ds
            unvisitedNeighbors =
              filter (\(n, _) -> not (HS.member n v1)) allNeighbors
         in processQueue $
              foldl
                (foldNeighbor node)
                (DijkstraState v1 d0 q1 grid_ bnd src dest)
                unvisitedNeighbors

foldNeighbor :: (Int, Int) -> DijkstraState -> ((Int, Int), Int) -> DijkstraState
foldNeighbor current_node ds@(DijkstraState v1 d0 q1 grid_ x2 x3 x4) (neighborNode, cost) =
  let current_height = HM.findWithDefault 0 current_node grid_
      altDistance = addDist (d0 !?? current_node) (Dist 1)--addDist (d0 !?? current_node) (Dist cost)
   in if (altDistance < d0 !?? neighborNode) && (cost <= (current_height + 1)) -- added condition here to limit step to 1
        then DijkstraState v1 (HM.insert neighborNode (altDistance,current_node) d0) (H.insert (altDistance, neighborNode) q1) grid_ x2 x3 x4
        else ds

findShortestDistance :: HashMap (Int, Int) Int -> (Int, Int) -> (Int, Int) -> (Int, Int, Int, Int) -> HashMap (Int, Int) (Distance Int, (Int,Int))
findShortestDistance grid src dest bnds = processQueue initialState -- processQueue initialState !?? dest
  where
    initialVisited = HS.empty
    initialDistances = HM.singleton src (Dist 0,src)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue grid bnds src dest

replaceSrcDest x
  | x == -14 = 0
  | x == -28 = 25
  | otherwise = x

piecePath :: HashMap (Int, Int) (Distance Int, (Int,Int)) -> (Int,Int) -> (Int,Int) -> [(Int,(Int,Int))]
piecePath d0 src dest 
  | dest == src = []
  | otherwise = (distance, parent) : piecePath d0 src parent
      where (Dist distance, parent) = fromJust $ HM.lookup dest d0
      
getNSteps :: HashMap (Int, Int) (Distance Int, (Int,Int)) -> (Int,Int) -> (Int,Int) -> Int
getNSteps dijk dest src = x
  where Dist x = fst $ fromMaybe (Dist 500, (-1,-1)) $ HM.lookup dest dijk

main = do
  contents <- readFile "data/day12.txt"
  let input_ = map (map (\x -> ord x - 97)) $ lines contents

      src = head [(y, x) | (y, line) <- zip ([0 ..] :: [Int]) input_, x <- elemIndices (-14) line]
      dest = head [(y, x) | (y, line) <- zip ([0 ..] :: [Int]) input_, x <- elemIndices (-28) line]
      grid = HM.fromList (concatMap (\(y, z) -> [((y, x), replaceSrcDest z) | (x, z) <- zip ([0 ..] :: [Int]) z]) $ zip ([0 ..] :: [Int]) input_)
      bnds = (0, length input_ - 1, 0, length (head input_) - 1)
      dijk = findShortestDistance grid src dest bnds
      final = getNSteps dijk dest src

      srcs = [(y, x) | (y, line) <- zip ([0 ..] :: [Int]) input_, x <- elemIndices 0 line]
      distances = map (\x -> getNSteps (findShortestDistance grid x dest bnds) dest x) srcs



  print $ "part 1: " ++ show final
  print $ "part 2: " ++ show (minimum distances)

