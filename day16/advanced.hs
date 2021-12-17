module Main where

import Data.Char
import System.Environment

import Utils.Binary (binaryToNumber, hexToBin)
import Utils.Parser
import Utils.Parsing (parseInt)

data Packet = Literal Int
            | Operator Operation [Packet] deriving (Eq, Show)

data Operation = Equals
               | GreaterThan
               | LessThan
               | Max
               | Min
               | Prod
               | Sum deriving (Eq, Show)

findOperation :: Int -> Operation
findOperation 0 = Sum
findOperation 1 = Prod
findOperation 2 = Min
findOperation 3 = Max
findOperation 5 = GreaterThan
findOperation 6 = LessThan
findOperation 7 = Equals

parse :: Parser Packet
parse = do {ret <- packet; _ <- many (token '0'); return ret}

packet :: Parser Packet
packet = literal <|> operator

literal :: Parser Packet
literal = do 
    _ <- chars 3
    _ <- string "100"
    Literal . binaryToNumber . concat <$> groups

operator :: Parser Packet
operator = operatorWithLength <|> operatorWithAmount

operatorWithLength :: Parser Packet
operatorWithLength = do
    _ <- chars 3
    t <- typeId
    _ <- token '0'
    length <- chars 15
    bits <- chars (binaryToNumber length)
    let packets = doParse (some packet) bits
    return $ Operator t packets

operatorWithAmount :: Parser Packet
operatorWithAmount = do
    _ <- chars 3
    t <- typeId
    _ <- token '1'
    amt <- chars 11
    packets <- replicateM (binaryToNumber amt) packet
    return $ Operator t packets

groups :: Parser [String]
groups = do {g <- many group; end <- close; return $ g ++ [end]}

close :: Parser String
close = do token '0'; chars 4

group :: Parser String
group = do token '1'; chars 4

typeId :: Parser Operation
typeId = do {v <- chars 3; return $ findOperation (binaryToNumber v)}

evaluate :: Packet -> Int
evaluate (Literal v) = v
evaluate (Operator Equals (a:b:_)) = if evaluate a == evaluate b then 1 else 0 
evaluate (Operator GreaterThan (a:b:_)) = if evaluate a > evaluate b then 1 else 0
evaluate (Operator LessThan (a:b:_)) = if evaluate a < evaluate b then 1 else 0
evaluate (Operator Max values) = maximum (map evaluate values)
evaluate (Operator Min values) = minimum (map evaluate values)
evaluate (Operator Prod values) = product (map evaluate values)
evaluate (Operator Sum values) = sum (map evaluate values)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (evaluate (doParse parse (concatMap hexToBin (head (lines contents)))))