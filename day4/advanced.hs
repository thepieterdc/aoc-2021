module Main where

import System.Environment

parseInt :: String -> Int
parseInt a = read a :: Int

filterNeg f = filter (not . f)

parseCell :: String -> String
parseCell = show . parseInt

data Configuration = Configuration [String] [Board]
                   deriving (Eq, Show)

type Board = [Row]

type Row = [String]

parse :: [String] -> Configuration
parse input = Configuration (parseGuesses (head input)) (parseBoards (tail (tail input)))

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards input = parseBoard (take 5 input) : parseBoards (drop 6 input)

parseBoard :: [String] -> Board
parseBoard = map parseRow

parseRow :: String -> Row
parseRow input = map parseCell (words input)

parseGuesses :: String -> [String]
parseGuesses input = parseCell x : next
    where 
    (x, next)
        | null rest = (x, [])
        | otherwise = (x, parseGuesses (tail rest))
        where (x, rest) = span (/= ',') input

runGame :: Configuration -> Maybe (Board, Int)
runGame (Configuration (move : moves) [board]) = if winner newBoard 
        then Just (newBoard, parseInt move)
        else runGame (Configuration moves [newBoard])
    where newBoard = applyMove move board
runGame (Configuration (move : moves) boards) = runGame (Configuration moves (filterNeg winner (map (applyMove move) boards)))

applyMove :: String -> Board -> Board
applyMove move = map (applyMoveToRow move)

applyMoveToRow :: String -> Row -> Row
applyMoveToRow move = map (\c -> if c == move then "X" else c)

findWinner :: [Board] -> Maybe Board
findWinner [] = Nothing
findWinner (board : boards)
    | winner board = Just board
    | otherwise = findWinner boards

winner :: Board -> Bool
winner board = winnerHorizontal board || winnerVertical board

winnerHorizontal :: Board -> Bool
winnerHorizontal = any winnerHorizontalRow

winnerHorizontalRow :: Row -> Bool
winnerHorizontalRow = all (== "X")

winnerVertical :: Board -> Bool
winnerVertical board = any (winnerVerticalColumn board) [0..(length (head board)-1)]

winnerVerticalColumn :: Board -> Int -> Bool
winnerVerticalColumn board column = all (winnerRowColumn column) board

winnerRowColumn :: Int -> Row -> Bool
winnerRowColumn column row = row !! column == "X"

result :: Maybe (Board, Int) -> Int
result Nothing = 0
result (Just (board, lastMove)) = boardSum board * lastMove

boardSum :: Board -> Int
boardSum board = sum (map rowSum board)

rowSum :: Row -> Int
rowSum row = sum (map parseInt (filter (/= "X") row))

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (result (runGame (parse (lines contents))))