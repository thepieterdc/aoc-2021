module Main where

import Data.List
import Data.Maybe
import System.Environment

data Token = Open Int | Close Int
           deriving (Show)

parse :: [String] -> [[Token]]
parse = map (map tokenize)

tokenize :: Char -> Token
tokenize '(' = Open 3
tokenize ')' = Close 3
tokenize '[' = Open 57
tokenize ']' = Close 57
tokenize '{' = Open 1197
tokenize '}' = Close 1197
tokenize '<' = Open 25137
tokenize '>' = Close 25137

run :: [[Token]] -> [Int]
run = map ((\(Close x) -> x) . fromMaybe (Close 0) . findInvalidToken [])

findInvalidToken :: [Token] -> [Token] -> Maybe Token
-- Processed the whole list as valid.
findInvalidToken _ [] = Nothing
-- Stack is empty but a close token was found.
findInvalidToken [] (Close x:_) = Just (Close x)
-- New open token was found, add it to the stack.
findInvalidToken stack (Open x:xs) = findInvalidToken (Open x : stack) xs
-- New close token was found.
findInvalidToken (Open s:stack) (Close x:xs) = if s == x
    -- Close token matches top of the stack, remove that and continue.
    then findInvalidToken stack xs
    -- Invalid close token was found, error.
    else Just (Close x)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (sum (run (parse (lines contents))))