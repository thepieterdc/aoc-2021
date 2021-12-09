module Main where

import System.Environment

import Utils.Stat (average, naturalSum)

window :: Int -> [Int]
window i = [(i - 20)..(i + 20)]

parseInt :: String -> Int
parseInt a = read a :: Int

parse :: String -> [Int]
parse input = parseInt x : (if rest /= [] then parse (tail rest) else [])
    where (x, rest) = span (/= ',') input

distance :: [Int] -> Int -> Int
distance inputs x = sum [naturalSum (abs(x - y)) | y <- inputs]

run :: [Int] -> Int
run input = minimum (map (distance input) (window (round (average (map fromIntegral input)))))

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (parse (head (lines contents))))