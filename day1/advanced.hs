module Main where

import System.Environment

import Utils.Parsing (parseInt)

run :: [Int] -> Int
run n = length (filter (uncurry (<)) (zip n (tail n ++ [0])))
        
slidingWindowify :: [Int] -> [Int]
slidingWindowify (a:b:c:rest) = slidingWindowifyHelper a b c rest []

slidingWindowifyHelper :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
slidingWindowifyHelper a b c (next:rest) result = slidingWindowifyHelper b c next rest (result ++ [a + b + c])
slidingWindowifyHelper a b c [] result = result ++ [a + b + c]

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (slidingWindowify (map parseInt (lines contents))))