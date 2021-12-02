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
        
main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    putStrLn $ show $ run (map parseInt (lines contents))