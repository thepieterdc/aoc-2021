module Main where

import Data.Char
import Data.List
import Data.Map (Map)
import Data.Ord
import qualified Data.Map as Map
import System.Environment

import Utils.Grid (Coordinate, getCoordinates, getOrthogonalNeighbours, Grid, Row, value)
import Utils.Parsing (parseCharToInt)

type Graph = Map Coordinate [(Coordinate, Int)]

parse :: [String] -> Grid Int
parse = map (map parseCharToInt)

mapNeighbours :: Grid Int -> Coordinate -> Graph
mapNeighbours grid coordinate = Map.singleton coordinate (map (\c -> (c, value grid c)) (getOrthogonalNeighbours grid coordinate))

run :: Grid Int -> Int
run grid = pathCost grid path target
    where (path, target) = run' (Map.unions (map (mapNeighbours grid) (getCoordinates grid)))

run' :: Graph -> (Map Coordinate Coordinate, Coordinate)
run' graph = (dijkstra graph (Map.insert (0, 0) 0 distances) (Map.keys graph) Map.empty target, target)
    where (distances, target) = (Map.mapWithKey (\k v -> 9999999999999) graph, fst (Map.findMax graph))

dijkstra :: 
    Graph -> 
    Map Coordinate Int -> -- distances map
    [Coordinate] -> -- queue of coordinates left to check
    Map Coordinate Coordinate -> -- previous coordinate
    Coordinate -> -- target
    Map Coordinate Coordinate
dijkstra _ _ [] prev _ = prev
dijkstra graph d q prev target = 
    let (closestNode, closestCost) = closest d q
    in if closestNode == target || closestCost == 9999999999999
        then prev
        else let (d', q', prev') = dijkstra' graph d q prev closestNode closestCost
             in dijkstra graph d' q' prev' target

dijkstra' :: 
    Graph -> 
    Map Coordinate Int -> 
    [Coordinate] -> 
    Map Coordinate Coordinate -> 
    Coordinate -> 
    Int -> 
    (Map Coordinate Int, [Coordinate], Map Coordinate Coordinate)
dijkstra' graph d q prev cN cC = (dijkstraD neighbours cC d, q \\ [cN], dijkstraPrev neighbours cN cC d prev)
    where neighbours = Map.findWithDefault [] cN graph

dijkstraD :: [(Coordinate, Int)] -> Int -> Map Coordinate Int -> Map Coordinate Int
dijkstraD [] _ ret = ret
dijkstraD ((c, cost):xs) prevCost ret = if prevCost + cost < Map.findWithDefault 9999999999999 c ret
    then dijkstraD xs prevCost (Map.insert c (prevCost + cost) ret)
    else dijkstraD xs prevCost ret

dijkstraPrev :: [(Coordinate, Int)] -> Coordinate -> Int -> Map Coordinate Int -> Map Coordinate Coordinate -> Map Coordinate Coordinate
dijkstraPrev [] _ _ _ ret = ret
dijkstraPrev ((c, cost):xs) prevNode prevCost d ret = if prevCost + cost < Map.findWithDefault 9999999999999 c d
    then dijkstraPrev xs prevNode prevCost d (Map.insert c prevNode ret)
    else dijkstraPrev xs prevNode prevCost d ret

closest :: Map Coordinate Int -> [Coordinate] -> (Coordinate, Int)
closest d q = minimumBy (comparing snd) (map (\c -> (c, Map.findWithDefault 9999999999999 c d)) q)

pathCost :: Grid Int -> Map Coordinate Coordinate -> Coordinate -> Int
pathCost _ _ (0, 0) = 0
pathCost grid prev cursor = value grid cursor + pathCost grid prev (Map.findWithDefault (0, 0) cursor prev)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (parse (lines contents)))