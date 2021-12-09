module Main where

import Data.Set(Set)
import qualified Data.Set as Set
import System.Environment

import Utils.Geometry.Base (Coordinate)
import Utils.Geometry.Line (getCoordinates, Line)
import Utils.Parsing (parseInt)
import Utils.Sorting (quicksort)

parse :: [String] -> [Line]
parse = map parseLine

parseLine :: String -> Line
parseLine row = (parsePoint start, parsePoint end)
    where (start, end) = (takeWhile (/= '-') row, tail (dropWhile (/= '>') row))

parsePoint :: String -> Coordinate
parsePoint part = (parseInt x, parseInt (tail y))
    where (x, y) = span (/= ',') part

generatePoints :: [Line] -> [[Coordinate]]
generatePoints = map getCoordinates

intersections :: [Coordinate] -> Set Coordinate -> [Coordinate]
intersections [] _ = []
intersections (x:xs) seen = if Set.member x seen
    then x : intersections xs seen
    else intersections xs (Set.insert x seen)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (Set.size (Set.fromList (intersections (concat (generatePoints (parse (lines contents)))) (Set.fromList []))))