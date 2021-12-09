module Main where

import System.Environment

import Utils.Parsing (parseInt)
import Utils.Sorting (quicksort)
import Utils.Stat (median)

parse :: String -> [Int]
parse input = parseInt x : (if rest /= [] then parse (tail rest) else [])
    where (x, rest) = span (/= ',') input

minDistance :: (Int, Int) -> [Int] -> Int
minDistance (a, b) inputs = if x < y then x else y
    where (x, y) = (distance a inputs, distance b inputs)

distance :: Int -> [Int] -> Int
distance x inputs = sum [abs(x - y) | y <- inputs]

run :: [Int] -> Int
run input = minDistance (median (quicksort input)) input

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (parse (head (lines contents))))