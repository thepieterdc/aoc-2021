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

findPaths :: Graph -> Cave -> Int -> [Path]
findPaths graph target maxDepth = findPathsRec graph (Set.singleton Start) Nothing maxDepth Start target

findPathsRec :: Graph -> Set Cave -> Maybe Cave -> Int -> Cave -> Cave -> [Path]
findPathsRec graph seen doubleSmall depth start target
    | depth == 0 = []
    | start == target = [[]]
    | otherwise = nub [nb:rest | entry <- newCandidates, nb <- Set.toList (fst entry), rest <- findPathsRec graph (Set.insert nb seen) (snd entry) (depth - 1) nb target]
    where newCandidates = Set.toList (candidates graph seen start doubleSmall)

candidates :: Graph -> Set Cave -> Cave -> Maybe Cave -> Set (Set Cave, Maybe Cave)
-- double small was already used, we can only visit large caves again.
candidates graph seen start (Just c) = Set.singleton (Set.difference (neighbours graph start) (Set.filter filterLarge seen), Just c)
-- attempt to use all small caves again, also include the option to not use a small cave this time.
candidates graph seen start Nothing = Set.insert (Set.difference (neighbours graph start) (Set.filter filterLarge seen), Nothing) (Set.map (candidatesHelper allNeighbours seen) allNeighbours)
    where allNeighbours = Set.delete Start (neighbours graph start)

candidatesHelper :: Set Cave -> Set Cave -> Cave -> (Set Cave, Maybe Cave)
-- use this case twice.
candidatesHelper all seen (Small a) = (Set.difference all (Set.filter filterLarge (Set.delete (Small a) all)), Just (Small a))
-- do not use this cave twice.
candidatesHelper all seen _ = (Set.difference all (Set.filter filterLarge seen), Nothing)

filterLarge :: Cave -> Bool
filterLarge (Large _) = False
filterLarge _ = True

neighbours :: Graph -> Cave -> Set Cave
neighbours graph x = snd (head (filter (\c -> fst c == x) graph))

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (length (findPaths (mapNeighbours (parse (lines contents))) End 16))