module Main where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

import Utils.Grid (Coordinate, floodFill, getCoordinates, Grid, value)
import Utils.Parsing (parseCharToInt)
import Utils.Sorting (quicksort)

parse :: [String] -> Grid Int
parse = map (map parseCharToInt)

run :: Grid Int -> [Set Coordinate]
run grid = runForCoordinates grid (getCoordinates grid) Set.empty

runForCoordinates :: Grid Int -> [Coordinate] -> Set Coordinate -> [Set Coordinate]
runForCoordinates _ [] _ = []
runForCoordinates grid (x:xs) seen
    | Set.member x seen = runForCoordinates grid xs seen
    | Set.null basin = runForCoordinates grid xs (Set.union seen basin)
    | otherwise = basin : runForCoordinates grid xs (Set.union seen basin)
    where basin = Set.fromList (findBasin grid [x] [])

findBasin :: Grid Int -> [Coordinate] -> [Coordinate] -> [Coordinate]
findBasin grid = floodFill (\c -> value grid c == 9) grid

findMaxBasins :: [Set Coordinate] -> [Int]
findMaxBasins inputs = take 3 (reverse (quicksort (map Set.size inputs)))

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (product (findMaxBasins (run (parse (lines contents)))))