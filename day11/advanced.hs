module Main where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

import Utils.Filtering (howMany)
import Utils.Grid (Coordinate, isNeighbour)
import Utils.Parsing (parseCharToInt)

type StateEntry = (Coordinate, Int, Bool)
type State = [StateEntry]

parse :: [String] -> [(Coordinate, Int, Bool)]
parse rows = concatMap (\(i, r) -> map (\(j, c) -> ((i, j), parseCharToInt c, False)) (zip [0..100] r)) (zip [0..100] rows)

run :: State -> Int -> Int
run _ 0 = 0
run state steps = 1 + if count == length state then 0 else run newState (steps - 1)
    where (newState, count) = step state

step :: State -> (State, Int)
step state = (reset result, countFlashes result)
    where result = calculateFlashes Set.empty (increaseEnergy state)

reset :: State -> State
reset = map (\(c, l, f) -> (c, if f then 0 else l, False))

countFlashes :: State -> Int
countFlashes = howMany (\(c, l, f) -> f)

increaseEnergy :: State -> State
increaseEnergy = map (\(c, l, b) -> (c, l + 1, b))

calculateFlashes :: Set Coordinate -> State -> State
calculateFlashes flashed state = if Set.null new
    then state
    else calculateFlashes (Set.union flashed new) (applyFlashesToState state new)
    where new = Set.difference (findFlashes state) flashed

findFlashes :: State -> Set Coordinate
findFlashes state = Set.fromList (map (\(c, i, f) -> c) (filter (\(c, i, f) -> not f && i > 9) state))

applyFlashesToState :: State -> Set Coordinate -> State
applyFlashesToState = foldl (flip applyFlashToState)

applyFlashToState :: Coordinate -> State -> State
applyFlashToState coordinate = map (applyFlashToCoordinate coordinate)

applyFlashToCoordinate :: Coordinate -> StateEntry -> StateEntry
applyFlashToCoordinate flash (c, l, f) = case (flash == c, isNeighbour flash c) of
    -- Flash current coordinate.
    (True, _) -> (c, l, True)
    -- Increase neighbours.
    (_, True) -> (c, l + 1, f)
    -- Leave the rest untouched.
    _ -> (c, l, f)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (run (parse (lines contents)) 10000)