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

instructionDepth :: Instruction -> Int
instructionDepth (Instruction Down x) = x
instructionDepth (Instruction Up x) = -x
instructionDepth _ = 0

instructionHoriz :: Instruction -> Int
instructionHoriz (Instruction Forward x) = x
instructionHoriz _ = 0

calculatePosition :: [Instruction] -> Int -> Int -> [Int]
calculatePosition (x:xs) d h = calculatePosition xs (d + instructionDepth x) (h + instructionHoriz x)
calculatePosition _ d h = [d, h]

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (product $ calculatePosition (map parseInstructionLine (lines contents)) 0 0)