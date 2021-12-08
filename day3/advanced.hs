module Main where

import System.Environment

lineLength :: [String] -> ([String], Int, Int)
lineLength x = (x, 0, length (head x))

findOxygen :: ([String], Int, Int) -> Int
findOxygen ([x], _, _) = binaryToNumber x
findOxygen (allLines, idx, max) = findOxygen (filter matcher allLines, idx + 1, max)
    where matcher l = (l !! idx) == maxIdx (runForIdx allLines idx 0 0)

findCO2 :: ([String], Int, Int) -> Int
findCO2 ([x], _, _) = binaryToNumber x
findCO2 (allLines, idx, max) = findCO2 (filter matcher allLines, idx + 1, max)
    where matcher l = (l !! idx) == minIdx (runForIdx allLines idx 0 0)

maxIdx :: (Int, Int) -> Char
maxIdx (zeros, ones)
    | zeros > ones = '0'
    | otherwise = '1'

minIdx :: (Int, Int) -> Char
minIdx (zeros, ones)
    | ones < zeros = '1'
    | otherwise = '0'

runForIdx :: [String] -> Int -> Int -> Int -> (Int, Int)
runForIdx (x : xs) idx zeros ones
    | (x !! idx) == '0' = runForIdx xs idx (zeros + 1) ones
    | otherwise = runForIdx xs idx zeros (ones + 1)
runForIdx [] _ zeros ones = (zeros, ones)

binaryToNumber :: String -> Int
binaryToNumber = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

mul :: (Int, Int) -> Int
mul (a, b) = a * b

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (mul (findOxygen (lineLength (lines contents)), findCO2(lineLength (lines contents))))