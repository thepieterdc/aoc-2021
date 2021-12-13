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

run :: (Set Coordinate, [Fold]) -> Set Coordinate
run (coordinates, []) = coordinates
run (coordinates, x:xs) = run (applyFolds x coordinates, xs)

output :: Set Coordinate -> [String]
output input = fill (maximum (Set.map fst input), maximum (Set.map snd input)) input

fill :: (Int, Int) -> Set Coordinate -> [String]
fill (cs, rs) coordinates = map (\r -> map (\c -> if Set.member (c, r) coordinates then '$' else ' ') [0..cs]) [0..rs]

printLines :: [String] -> IO ()
printLines [] = putStr ""
printLines (x:xs) = do putStrLn x; printLines xs

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    printLines (output (run (parse (lines contents))))