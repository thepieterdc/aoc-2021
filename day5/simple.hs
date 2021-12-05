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
parse input = map parseLine input

parseLine :: String -> Line
parseLine row = (parsePoint start, parsePoint end)
    where (start, end) = ((takeWhile (/= '-') row), tail (dropWhile (/= '>') row))

parsePoint :: String -> Point
parsePoint part = (parseInt x, parseInt (tail y))
    where (x, y) = span (/= ',') part

filterDiag :: [Line] -> [Line]
filterDiag = filterNeg isDiagonal

isDiagonal :: Line -> Bool
isDiagonal ((x1, y1), (x2, y2)) = (x1 /= x2) && (y1 /= y2)

generatePoints :: [Line] -> [[Point]]
generatePoints = map generatePointsForLine

generatePointsForLine :: Line -> [Point]
generatePointsForLine ((x1, y1), (x2, y2)) = 
    if (x1 == x2) 
        then (generateVerticalLine (x1, y1) (x2, y2))
        else (generateHorizontalLine (x1, y1) (x2, y2))

generateHorizontalLine :: Point -> Point -> [Point]
generateHorizontalLine (x1, y) (x2, _)
    | (x1 == x2) = [(x2, y)]
    | (x1 < x2) = [(x1, y)] ++ generateHorizontalLine (x1 + 1, y) (x2, y)
    | otherwise = [(x1, y)] ++ generateHorizontalLine (x1 - 1, y) (x2, y)

generateVerticalLine :: Point -> Point -> [Point]
generateVerticalLine (x, y1) (_, y2)
    | (y1 == y2) = [(x, y2)]
    | (y1 < y2) = [(x, y1)] ++ generateVerticalLine (x, y1 + 1) (x, y2)
    | otherwise = [(x, y1)] ++ generateVerticalLine (x, y1 - 1) (x, y2)

flatten :: [[Point]] -> [Point]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

frequencies :: [Point] -> Maybe (Point, Int) -> [(Point, Int)]
frequencies [] (Just (p, i)) = [(p, i)]
frequencies (x:xs) Nothing = frequencies xs (Just (x, 1))
frequencies (x:xs) (Just (p, i))
    | (x == p) = frequencies xs (Just (x, i + 1))
    | otherwise = [(p, i)] ++ (frequencies xs (Just (x, 1)))

filterMultiple :: [(Point, Int)] -> [(Point, Int)]
filterMultiple = filter (\p -> (snd p) > 1)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    putStrLn $ show $ length $ filterMultiple (frequencies (quicksort (flatten (generatePoints (filterDiag (parse (lines contents)))))) Nothing)