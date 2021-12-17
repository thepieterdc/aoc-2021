module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

import Utils.Grid (Coordinate)
import Utils.Parsing (parseInt)

type Bounds = (Coordinate, Coordinate)

parse :: String -> Bounds
parse input = let (xs, ys) = span (/= ',') (drop 13 input) in
              let ((xmin, xmax), (ymin, ymax)) = (parseValues xs, parseValues (drop 2 ys)) in
              ((xmin, ymin), (xmax, ymax))

parseValues :: String -> (Int, Int)
parseValues input = (parseInt min, parseInt (drop 2 max))
    where (min, max) = span (/= '.') (drop 2 input)

find :: Bounds -> Int
find ((minX, minY), (maxX, maxY)) = length [(x, y) | x <- [0..maxX], y <- [minY..(abs minY - 1)], hit ((minX, minY), (maxX, maxY)) (x, y)]

hit :: Bounds -> (Int, Int) -> Bool
hit = hit' (0, 0)

hit' :: (Int, Int) -> Bounds -> (Int, Int) -> Bool
hit' (x, y) ((minX,minY), (maxX,maxY)) (vX, vY) = case (x <= maxX, x >= minX, y <= maxY, y >= minY) of
    (True, True, True, True) -> True
    (True, _, _, True) -> hit' (x + vX, y + vY) ((minX,minY), (maxX, maxY)) (if vX == 0 then 0 else vX - 1, vY - 1)
    (_, _, _, _) -> False

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (find (parse (head (lines contents))))