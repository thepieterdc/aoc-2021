module Main where

import Data.List
import System.Environment

import Utils.Filtering (filterByLength, filterContainsSubString)
import Utils.Sorting (quicksort)

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
mapNumbers all (_, "", _, _, _, _, _, _, _, _) = mapNumbers (all \\ [one, four, seven, eight]) ("", one, "", "", four, "", "", seven, eight, "")
    where (one, four, seven, eight) = (findOne all, findFour all, findSeven all, findEight all)
-- number 9 can be found by merging 4 and 7 and finding the missing bar at the bottom.
mapNumbers all (zero, one, two, three, four, five, six, seven, eight, "") = mapNumbers (all \\ [nine]) (zero, one, two, three, four, five, six, seven, eight, nine)
    where nine = findNine all four seven
-- number 0 has 6 components and contains the 7.
mapNumbers all ("", one, two, three, four, five, six, seven, eight, nine) = mapNumbers (all \\ [zero]) (zero, one, two, three, four, five, six, seven, eight, nine)
    where zero = findZero all seven
-- number 6 has 6 components.
mapNumbers all (zero, one, two, three, four, five, "", seven, eight, nine) = mapNumbers (all \\ [six]) (zero, one, two, three, four, five, six, seven, eight, nine)
    where six = findSix all
-- number 5 contains the 6 minus the part at the left-bottom.
mapNumbers all (zero, one, two, three, four, "", six, seven, eight, nine) = mapNumbers (all \\ [five]) (zero, one, two, three, four, five, six, seven, eight, nine)
    where five = findFive all six
-- number 3 contains the 9 minus the part at the left-top.
mapNumbers all (zero, one, two, "", four, five, six, seven, eight, nine) = mapNumbers (all \\ [three]) (zero, one, two, three, four, five, six, seven, eight, nine)
    where three = findThree all nine
-- number 2 is the only remaining one.
mapNumbers all (zero, one, "", three, four, five, six, seven, eight, nine) = (zero, one, head all, three, four, five, six, seven, eight, nine)

findZero :: [String] -> Seven -> Zero
findZero all seven = head (filterContainsSubString seven (filterByLength 6 all))

findOne :: [String] -> One
findOne = head . filterByLength 2

findThree :: [String] -> Nine -> Three
findThree all nine = head (filter (\d -> length (d `intersect` nine) == 5) all)

findFour :: [String] -> Four
findFour = head . filterByLength 4

findFive :: [String] -> Six -> Five
findFive all six = head (filter (\d -> length (d `intersect` six) == 5) all)

findSix :: [String] -> Six
findSix = head . filterByLength 6

findSeven :: [String] -> Seven
findSeven = head . filterByLength 3

findEight :: [String] -> Eight
findEight = head . filterByLength 7

findNine :: [String] -> Four -> Seven -> Nine
findNine all four seven = head (filterContainsSubString (four `union` seven) all)

run :: [Observation] -> Int
run = sum . map runObservation

runObservation :: Observation -> Int
runObservation (digits, combination) = numberify (map (translate mapping) combination)
    where mapping = mapNumbers digits ("", "", "", "", "", "", "", "", "", "")

numberify :: [Int] -> Int
numberify (a:b:c:d:_) = a * 1000 + b * 100 + c * 10 + d

translate :: Digits -> String -> Int
translate (zero, one, two, three, four, five, six, seven, eight, nine) value
    | value == one = 1
    | value == two = 2
    | value == three = 3
    | value == four = 4
    | value == five = 5
    | value == six = 6
    | value == seven = 7
    | value == eight = 8
    | value == nine = 9
    | otherwise = 0

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (parse (lines contents)))