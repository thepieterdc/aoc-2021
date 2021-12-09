module Main where

import System.Environment

import Utils.Binary (binaryFlip, binaryToNumber)

lineLength :: [String] -> ([String], Int)
lineLength x = (x, length (head x))

run :: ([String], Int) -> String
run (allLines, 0) = ""
run (allLines, idx) = run (allLines, idx - 1) ++ maxIdx (runForIdx allLines (idx - 1) 0 0)

maxIdx :: (Int, Int) -> String
maxIdx (zeros, ones)
    | zeros > ones = "0"
    | otherwise = "1"

runForIdx :: [String] -> Int -> Int -> Int -> (Int, Int)
runForIdx (x : xs) idx zeros ones
    | (x !! idx) == '0' = runForIdx xs idx (zeros + 1) ones
    | otherwise = runForIdx xs idx zeros (ones + 1)
runForIdx [] _ zeros ones = (zeros, ones)

binaryInterpret :: String -> [Int]
binaryInterpret i = [binaryToNumber i, binaryToNumber (binaryFlip i)]

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (product $ binaryInterpret (run (lineLength (lines contents))))