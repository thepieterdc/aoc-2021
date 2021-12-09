module Main where

import System.Environment

import Utils.Parsing (parseInt)
        
run :: [Int] -> Int
run n = length (filter (uncurry (<)) (zip n (tail n ++ [0])))

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (map parseInt (lines contents)))