module Main where

import Data.List
import System.Environment

type Coordinate = (Int, Int)
type Grid = [Row]
type Row = [Int]

parseInt :: Char -> Int
parseInt a = read [a] :: Int

parse :: [String] -> Grid
parse = map (map parseInt)

getCoordinates :: Grid -> [Coordinate]
getCoordinates grid = [(r, c) | r <- [0..length grid - 1], c <- [0..length (head grid) - 1]]

findLowPoints :: Grid -> [Coordinate]
findLowPoints grid = filter (\(r, c) -> value grid (r, c) < getLowestNeighbour grid (r, c)) (getCoordinates grid)

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

getLowestNeighbour :: Grid -> Coordinate -> Int
getLowestNeighbour grid (r, c) = minimum (map (\(r', c') -> value grid (r', c')) (getNeighbours grid (r, c)))

value :: Grid -> Coordinate -> Int
value grid (r, c) = (grid !! r) !! c

run :: Grid -> Int
run grid = sum (map ((+1) . value grid) (findLowPoints grid))

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (parse (lines contents)))