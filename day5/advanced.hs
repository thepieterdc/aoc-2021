module Main where

import System.Environment

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

type Point = (Int, Int)

type Line = (Point, Point)

filterNeg f = filter (not . f)

parseInt :: String -> Int
parseInt a = read a :: Int

parse :: [String] -> [Line]
parse = map parseLine

parseLine :: String -> Line
parseLine row = (parsePoint start, parsePoint end)
    where (start, end) = (takeWhile (/= '-') row, tail (dropWhile (/= '>') row))

parsePoint :: String -> Point
parsePoint part = (parseInt x, parseInt (tail y))
    where (x, y) = span (/= ',') part

generatePoints :: [Line] -> [[Point]]
generatePoints = map generatePointsForLine

generatePointsForLine :: Line -> [Point]
generatePointsForLine ((x1, y1), (x2, y2)) = case (x1 == x2, y1 == y2) of
    (True, _) -> generateVerticalLine (x1, y1) (x2, y2)
    (_, True) -> generateHorizontalLine (x1, y1) (x2, y2)
    _ -> generateDiagonalLine (x1, y1) (x2, y2)

generateDiagonalLine :: Point -> Point -> [Point]
generateDiagonalLine (x1, y1) (x2, y2)
    | x1 == x2 = [(x2, y2)]
    | x1 < x2 && y1 < y2 = (x1, y1) : generateDiagonalLine (x1 + 1, y1 + 1) (x2, y2)
    | x1 < x2 && y1 > y2 = (x1, y1) : generateDiagonalLine (x1 + 1, y1 - 1) (x2, y2)
    | x1 > x2 && y1 < y2 = (x1, y1) : generateDiagonalLine (x1 - 1, y1 + 1) (x2, y2)
    | otherwise = (x1, y1) : generateDiagonalLine (x1 - 1, y1 - 1) (x2, y2)

generateHorizontalLine :: Point -> Point -> [Point]
generateHorizontalLine (x1, y) (x2, _)
    | x1 == x2 = [(x2, y)]
    | x1 < x2 = (x1, y) : generateHorizontalLine (x1 + 1, y) (x2, y)
    | otherwise = (x1, y) : generateHorizontalLine (x1 - 1, y) (x2, y)

generateVerticalLine :: Point -> Point -> [Point]
generateVerticalLine (x, y1) (_, y2)
    | y1 == y2 = [(x, y2)]
    | y1 < y2 = (x, y1) : generateVerticalLine (x, y1 + 1) (x, y2)
    | otherwise = (x, y1) : generateVerticalLine (x, y1 - 1) (x, y2)

frequencies :: [Point] -> Maybe (Point, Int) -> [(Point, Int)]
frequencies [] (Just (p, i)) = [(p, i)]
frequencies (x:xs) Nothing = frequencies xs (Just (x, 1))
frequencies (x:xs) (Just (p, i))
    | x == p = frequencies xs (Just (x, i + 1))
    | otherwise = (p, i) : frequencies xs (Just (x, 1))

filterMultiple :: [(Point, Int)] -> [(Point, Int)]
filterMultiple = filter (\p -> snd p > 1)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (length $ filterMultiple (frequencies (quicksort (concat (generatePoints (parse (lines contents))))) Nothing))