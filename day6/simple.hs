module Main where

import System.Environment

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

parseInt :: String -> Int
parseInt a = read a :: Int

parse :: String -> [Int]
parse input = [parseInt x] ++ (if (rest /= []) then parse (tail rest) else [])
    where (x, rest) = span (/= ',') input

run :: [Int] -> Int -> [Int]
run input 0 = input
run input amt = run (flatten (map handleFish input)) (amt - 1)

handleFish :: Int -> [Int]
handleFish 0 = [6, 8]
handleFish i = [i - 1]

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    putStrLn $ show $ length $ run (parse (head (lines contents))) 80