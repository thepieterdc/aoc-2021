module Main where

import System.Environment

parseInt :: String -> Int
parseInt a = read a :: Int

run :: [Int] -> Int
run (first:rest) = runRec first rest 0

runRec :: Int -> [Int] -> Int -> Int
runRec _ [] amount = amount
runRec prev (current:rest) amount = runRec current rest newAmt
    where
    newAmt
        | current > prev = amount + 1
        | otherwise = amount
        
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