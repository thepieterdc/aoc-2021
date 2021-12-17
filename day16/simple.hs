module Main where

import Data.Char
import System.Environment

import Utils.Binary (binaryToNumber, hexToBin)
import Utils.Parser
import Utils.Parsing (parseInt)

data Packet = Literal Int
            | Operator Int [Packet] deriving (Eq, Show)

parse :: Parser Packet
parse = do {ret <- packet; _ <- many (token '0'); return ret}

packet :: Parser Packet
packet = literal <|> operator

literal :: Parser Packet
literal = do 
    v <- version
    _ <- string "100"
    _ <- groups
    return $ Literal v

operator :: Parser Packet
operator = operatorWithLength <|> operatorWithAmount

operatorWithLength :: Parser Packet
operatorWithLength = do
    v <- version
    _ <- chars 3
    _ <- token '0'
    length <- chars 15
    raw <- chars (binaryToNumber length)
    let packets = doParse (some packet) raw
    return $ Operator v packets

operatorWithAmount :: Parser Packet
operatorWithAmount = do
    v <- version
    _ <- chars 3
    _ <- token '1'
    amt <- chars 11
    packets <- replicateM (binaryToNumber amt) packet
    return $ Operator v packets

groups :: Parser [String]
groups = do {g <- many group; end <- close; return $ g ++ [end]}

close :: Parser String
close = do token '0'; chars 4

group :: Parser String
group = do token '1'; chars 4

version :: Parser Int
version = do {v <- chars 3; return $ binaryToNumber v}

packetSum :: Packet -> Int
packetSum (Literal v) = v
packetSum (Operator v pkgs) = v + sum (map packetSum pkgs)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (packetSum (doParse parse (concatMap hexToBin (head (lines contents)))))