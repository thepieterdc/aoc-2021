module Main where

import System.Environment

import Utils.Parsing (parseInt)

getMinY :: String -> Int
getMinY input = let yVal = drop 2 (dropWhile (/= 'y') input) in
                let (min, max) = span (/= '.') yVal in
                parseInt min

run :: Int -> Int
run minY = sum [1..(abs minY - 1)]

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (getMinY (head (lines contents))))