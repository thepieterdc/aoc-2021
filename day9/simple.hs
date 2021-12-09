module Main where

import Data.List
import System.Environment

import Utils.Grid (Coordinate, getCoordinates, getOrthogonalNeighbours, Grid, Row, value)
import Utils.Parsing (parseCharToInt)

parse :: [String] -> Grid Int
parse = map (map parseCharToInt)

findLowPoints :: Grid Int -> [Coordinate]
findLowPoints grid = filter (\(r, c) -> value grid (r, c) < getLowestNeighbour grid (r, c)) (getCoordinates grid)

getLowestNeighbour :: Grid Int -> Coordinate -> Int
getLowestNeighbour grid (r, c) = minimum (map (\(r', c') -> value grid (r', c')) (getOrthogonalNeighbours grid (r, c)))

run :: Grid Int -> Int
run grid = sum (map ((+1) . value grid) (findLowPoints grid))

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (parse (lines contents)))