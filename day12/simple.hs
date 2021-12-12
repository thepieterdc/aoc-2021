module Main where

import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

import Utils.Graph (Edge)

type Graph = [(Cave, Set Cave)]
data Cave = Start | End | Small String | Large String
          deriving (Eq, Show)
type Path = [Cave]

instance Ord Cave where
    Start <= _ = True
    _ <= End = True
    Small _ <= Large _ = True
    Small a <= Small b = a <= b
    Large a <= Large b = a <= b
    _ <= _ = False

parse:: [String] -> [(Cave, Cave)]
parse = map parseEdge

parseEdge :: String -> (Cave, Cave)
parseEdge edge = (parseNode start, parseNode (tail end))
    where (start, end) = span (/= '-') edge

parseNode :: String -> Cave
parseNode "end" = End
parseNode "start" = Start
parseNode other = if all isUpper other
    then Large other
    else Small other

mapNeighbours :: [(Cave, Cave)] -> Graph
mapNeighbours edges = foldl addNeighbour (nub (concatMap (\(a, b) -> [(a, Set.empty), (b, Set.empty)]) edges)) edges

addNeighbour :: Graph -> (Cave, Cave) -> Graph
addNeighbour [] _ = []
addNeighbour ((x, nbs):xs) (a, b) = case (x == a, x == b) of
    (True, _) -> (x, Set.insert b nbs) : addNeighbour xs (a, b)
    (_, True) -> (x, Set.insert a nbs) : addNeighbour xs (a, b)
    _ -> (x, nbs) : addNeighbour xs (a, b)

findPaths :: Graph -> Cave -> Int -> Int
findPaths graph target maxDepth = findPathsRec graph (Set.singleton Start) maxDepth Start target

findPathsRec :: Graph -> Set Cave -> Int -> Cave -> Cave -> Int
findPathsRec graph seen depth start target
    | start == target = 1
    | depth == 0 = 0
    | otherwise = sum (map (\n -> findPathsRec graph (Set.insert n seen) (depth - 1) n target) newCandidates)
    where newCandidates = Set.toList (Set.difference (neighbours graph start) (Set.filter filterLarge seen))

filterLarge :: Cave -> Bool
filterLarge (Large _) = False
filterLarge _ = True

neighbours :: Graph -> Cave -> Set Cave
neighbours graph x = snd (head (filter (\c -> fst c == x) graph))

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (findPaths (mapNeighbours (parse (lines contents))) End 50)