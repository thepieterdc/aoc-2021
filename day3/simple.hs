module Main where

import System.Environment

lineLength :: [String] -> ([String], Int)
lineLength x = (x, length (head x))

run :: ([String], Int) -> String
run (allLines, 0) = ""
run (allLines, idx) = run (allLines, idx - 1) ++ maxIdx (runForIdx allLines (idx - 1) 0 0)

maxIdx :: (Int, Int) -> String
maxIdx (zeros, ones)
    | zeros > ones = "0"
    | otherwise = "1"

runForIdx :: [[Char]] -> Int -> Int -> Int -> (Int, Int)
runForIdx (x : xs) idx zeros ones
    | ((x !! idx) == '0') = runForIdx xs idx (zeros + 1) ones
    | otherwise = runForIdx xs idx zeros (ones + 1)
runForIdx [] _ zeros ones = (zeros, ones)

binaryInterpret :: String -> (Int, Int)
binaryInterpret i = (binaryToNumber i, binaryToNumber (binaryFlip i))

binaryFlip :: String -> String
binaryFlip xs = map flip xs
    where flip x = if x == '0' then '1' else '0'

binaryToNumber :: String -> Int
binaryToNumber = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

mul :: (Int, Int) -> Int
mul (a, b) = a * b

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    putStrLn $ show $ mul $ binaryInterpret (run ((lineLength (lines contents))))