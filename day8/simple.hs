module Main where

import Data.List
import System.Environment

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- Increase legibility.
type Zero = String
type One = String
type Two = String
type Three = String
type Four = String
type Five = String
type Six = String
type Seven = String
type Eight = String
type Nine = String

type Digits = (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine)
type Combination = [String]
type Observation = ([String], Combination)

parse :: [String] -> [Observation]
parse = map parseObservation

parseObservation :: String -> Observation
parseObservation input = (parseInput a, parseInput (tail (tail b)))
    where (a, b) = span (/= '|') input

parseInput :: String -> [String]
parseInput input = quicksort a : if b /= [] then parseInput (tail b) else []
    where (a, b) = span (/= ' ') input

mapNumbers :: [String] -> Digits -> Digits
-- numbers 1, 4, 7, 8 can be found immediately since the amount of components is unique.
mapNumbers all _ = ("", findOne all, "", "", findFour all, "", "", findSeven all, findEight all, "")

findOne :: [String] -> One
findOne = head . filterByLength 2

findFour :: [String] -> Four
findFour = head . filterByLength 4

findSeven :: [String] -> Seven
findSeven = head . filterByLength 3

findEight :: [String] -> Eight
findEight = head . filterByLength 7

filterByLength :: Int -> [String] -> [String]
filterByLength len = filter (\d -> length d == len)

run :: [Observation] -> Int
run = sum . map runObservation

runObservation :: Observation -> Int
runObservation (digits, combination) = sum (map (exists mapping) combination)
    where mapping = mapNumbers digits ("", "", "", "", "", "", "", "", "", "")

exists :: Digits -> String -> Int
exists (_, one, _, _, four, _, _, seven, eight, _) value
    | value == one = 1
    | value == four = 1
    | value == seven = 1
    | value == eight = 1
    | otherwise = 0

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (parse (lines contents)))