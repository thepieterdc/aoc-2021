module Main where

import System.Environment

average :: [Double] -> Double
average inputs = sum inputs / (fromIntegral (length inputs))

window :: Int -> [Int]
window i = [(i - 20)..(i + 20)]

parseInt :: String -> Int
parseInt a = read a :: Int

parse :: String -> [Int]
parse input = [parseInt x] ++ (if (rest /= []) then parse (tail rest) else [])
    where (x, rest) = span (/= ',') input

distance :: [Int] -> Int -> Int
distance inputs x = sum [sumNaturals (abs(x - y)) | y <- inputs]

sumNaturals :: Int -> Int
sumNaturals n = round ((fromIntegral (n * (n + 1))) / (fromIntegral 2))

minList :: [Int] -> Int
minList [] = 0
minList [x] = x
minList (x:y:xs)
    | x > y = minList (y:xs)
    | x < y = minList (x:xs)
    | otherwise = minList (x:xs)

run :: [Int] -> Int
run input = minList (map (distance input) (window (round (average (map fromIntegral input)))))

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    putStrLn $ show $ run (parse (head (lines contents)))