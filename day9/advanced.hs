module Main where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

type Coordinate = (Int, Int)
type Grid = [Row]
type Row = [Int]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

parseInt :: Char -> Int
parseInt a = read [a] :: Int

parse :: [String] -> Grid
parse = map (map parseInt)

getCoordinates :: Grid -> [Coordinate]
getCoordinates grid = [(r, c) | r <- [0..length grid - 1], c <- [0..length (head grid) - 1]]

getNeighbours :: Grid -> Coordinate -> [Coordinate]
getNeighbours grid (r, c) = case (r, c, r + 1 == length grid, c + 1 == length (head grid)) of
    -- 4 corners.
    (0, 0, _, _) -> [(0, 1), (1, 0)]
    (_, _, True, True) -> [(r, c - 1), (r - 1, c)]
    (_, 0, True, _) -> [(r, 1), (r - 1, 0)]
    (0, _, _, True) -> [(0, c - 1), (1, c)]
    -- 4 edges.
    (0, _, _, _) -> [(0, c - 1), (0, c + 1), (1, c)]
    (_, _, _, True) -> [(r - 1, c), (r + 1, c), (r, c - 1)]
    (_, _, True, _) -> [(r, c - 1), (r, c + 1), (r - 1, c)]
    (_, 0, _, _) -> [(r - 1, c), (r + 1, c), (r, c + 1)]
    -- Everywhere else.
    (_, _, _, _) -> [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

value :: Grid -> Coordinate -> Int
value grid (r, c) = (grid !! r) !! c

run :: Grid -> [Set Coordinate]
run grid = runForCoordinates grid (getCoordinates grid) (Set.fromList [])

runForCoordinates :: Grid -> [Coordinate] -> Set Coordinate -> [Set Coordinate]
runForCoordinates _ [] _ = []
runForCoordinates grid (x:xs) seen
    | Set.member x seen = runForCoordinates grid xs seen
    | Set.null basin = runForCoordinates grid xs (Set.union seen basin)
    | otherwise = basin : runForCoordinates grid xs (Set.union seen basin)
    where basin = Set.fromList (findBasin grid [x] [])

findBasin :: Grid -> [Coordinate] -> [Coordinate] -> [Coordinate]
findBasin _ [] seen = []
findBasin grid (x:todo) seen = if value grid x == 9
    then findBasin grid todo seen
    else x : findBasin grid (todo `union` (getNeighbours grid x \\ seen)) (x : seen) 

findMaxBasins :: [Set Coordinate] -> [Int]
findMaxBasins inputs = take 3 (reverse (quicksort (map Set.size inputs)))

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (product (findMaxBasins (run (parse (lines contents)))))