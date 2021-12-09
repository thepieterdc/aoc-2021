module Main where

import System.Environment

import Utils.Parsing (parseInt)

data Direction = Down | Forward | Up
               deriving (Eq, Show)

parseDirection :: String -> Direction
parseDirection "down" = Down
parseDirection "forward" = Forward
parseDirection "up" = Up

data Instruction = Instruction Direction Int
                 deriving (Eq, Show)

parseInstructionLine :: String -> Instruction
parseInstructionLine input = Instruction (parseDirection (head parts)) (parseInt (head (tail parts)))
    where parts = words input

diffAim :: Instruction -> Int
diffAim (Instruction Down x) = x
diffAim (Instruction Up x) = -x
diffAim _ = 0

diffDepth :: Instruction -> Int -> Int
diffDepth (Instruction Forward x) aim = x * aim
diffDepth _ _ = 0

diffHoriz :: Instruction -> Int
diffHoriz (Instruction Forward x) = x
diffHoriz _ = 0

calculatePosition :: [Instruction] -> Int -> Int -> Int -> [Int]
calculatePosition (x:xs) d h aim = calculatePosition xs (d + diffDepth x aim) (h + diffHoriz x) (aim + diffAim x)
calculatePosition _ d h _ = [d, h]

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (product $ calculatePosition (map parseInstructionLine (lines contents)) 0 0 0)