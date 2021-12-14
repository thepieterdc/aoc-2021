module Main where

import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment

import Utils.Counter (Counter, increment, maxValue, minValue)

type ReplaceMap = Map String (String, String)

parse :: [String] -> (Counter String, ReplaceMap)
parse l = (parseTemplate (head l), Map.fromList (map parseReplacement (tail (tail l))))

parseTemplate :: String -> Counter String
parseTemplate tpl = Map.unionsWith (+) maps
    where maps = [Map.singleton [tpl !! (i - 1), tpl !! i] 1 | i <- [1..length tpl - 1]]

parseReplacement :: String -> (String, (String, String))
parseReplacement input = ([head left, left !! 1], ([head left, last right], [last right, left !! 1]))
    where (left, right) = span (/= ' ') input

run :: (Counter String, ReplaceMap) -> Int -> Counter String
run (counter, _) 0 = counter
run (counter, replace) steps = run (Map.unionsWith (+) (concatMap (polyMap replace) (Map.assocs counter)), replace) (steps - 1)

polyMap :: ReplaceMap -> (String, Int) -> [Counter String]
polyMap replace (input, occs) = [Map.singleton a occs, Map.singleton b occs]
    where (a, b) = Map.findWithDefault (input, input) input replace

countChars :: Counter String -> Counter Char
countChars tuples = Map.unionsWith (+) (concatMap countCharsSub (Map.assocs tuples))

countCharsSub :: (String, Int) -> [Counter Char]
countCharsSub (poly, amount) = [Map.singleton (head poly) amount, Map.singleton (last poly) amount] 

calculate :: Counter Char -> Int
calculate counter = (maxValue counter - minValue counter) `div` 2

-- Increment the start and end of the template string by 1 because these are never counted.
incStartEnd :: String -> Counter Char -> Counter Char
incStartEnd template counter = increment (head template) (increment (last template) counter)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (calculate (incStartEnd (head (lines contents)) (countChars (run (parse (lines contents)) 40))))