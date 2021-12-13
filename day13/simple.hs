module Main where

import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

import Utils.Grid (Coordinate)
import Utils.Parsing (parseInt)

data Fold = Vertical Int | Horizontal Int
          deriving (Eq, Show)

parse :: [String] -> (Set Coordinate, [Fold])
parse lines = (Set.fromList (map parseCoordinate coordinates), map (parseFold . span (/= '=') . last . words) (tail folds))
    where (coordinates, folds) = span (/= "") lines

parseCoordinate :: String -> Coordinate
parseCoordinate raw = (parseInt x, parseInt (tail y))
    where (x, y) = span (/= ',') raw

parseFold :: (String, String) -> Fold
parseFold ("x", a) = Vertical (parseInt (tail a))
parseFold ("y", a) = Horizontal (parseInt (tail a))

applyFolds :: Fold -> Set Coordinate -> Set Coordinate
applyFolds f = Set.map (applyFold f)

applyFold :: Fold -> Coordinate -> Coordinate
applyFold (Horizontal line) (x, y) = if y < line
    then (x, y)
    else (x, line - (y - line))
applyFold (Vertical line) (x, y) = if x < line
    then (x, y)
    else (line - (x - line), y)

run :: (Set Coordinate, [Fold]) -> Int
run (coordinates, x:xs) = length (applyFolds x coordinates)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (parse (lines contents)))