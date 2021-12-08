module Main where

import System.Environment

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

parseInt :: String -> Int
parseInt a = read a :: Int

parse :: String -> [Int]
parse input = parseInt x : (if rest /= [] then parse (tail rest) else [])
    where (x, rest) = span (/= ',') input

median :: [Int] -> (Int, Int)
median [x] = (x, x)
median [x, y] = (x, y)
median (x:y:xs) = median (tail (reverse xs))

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