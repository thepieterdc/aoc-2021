module Main where

import Data.List
import System.Environment

import Utils.Sorting (quicksort)
import Utils.Stat (median)

data Token = Open Int | Close Int
           deriving (Show)

parse :: [String] -> [[Token]]
parse = map (map tokenize)

tokenize :: Char -> Token
tokenize '(' = Open 1
tokenize ')' = Close 1
tokenize '[' = Open 2
tokenize ']' = Close 2
tokenize '{' = Open 3
tokenize '}' = Close 3
tokenize '<' = Open 4
tokenize '>' = Close 4

run :: [[Token]] -> [Int]
run = map (calculatePoints . autocomplete [])

calculatePoints :: [Token] -> Int
calculatePoints = foldl (\a (Close b) -> a * 5 + b) 0

autocomplete :: [Token] -> [Token] -> [Token]
-- Input is valid and complete.
autocomplete [] [] = []
-- Input is valid but incomplete, perform autocompletion.
autocomplete (Open s:stack) [] = Close s : autocomplete stack []
-- Stack is empty but a close token was found, input is invalid.
autocomplete [] (Close x:_) = []
-- New open token was found, add it to the stack.
autocomplete stack (Open x:xs) = autocomplete (Open x : stack) xs
-- New close token was found.
autocomplete (Open s:stack) (Close x:xs) = if s == x
    -- Close token matches top of the stack, remove that and continue.
    then autocomplete stack xs
    -- Invalid close token was found.
    else []

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (fst (median (quicksort (filter (/= 0) (run (parse (lines contents)))))))