module Main where

import System.Environment

type State = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

parse :: String -> State -> State
parse "" state = state
parse input state = parse (if rest /= [] then tail rest else []) (incrementState x state)
    where (x, rest) = span (/= ',') input

incrementState :: String -> State -> State
incrementState input (zero, one, two, three, four, five, six, seven, eight) = (zero', one', two', three', four', five', six', seven', eight')
    where zero' = if input == "0" then zero + 1 else zero
          one' = if input == "1" then one + 1 else one
          two' = if input == "2" then two + 1 else two
          three' = if input == "3" then three + 1 else three
          four' = if input == "4" then four + 1 else four
          five' = if input == "5" then five + 1 else five
          six' = if input == "6" then six + 1 else six
          seven' = if input == "7" then seven + 1 else seven
          eight' = if input == "8" then eight + 1 else eight

run :: State -> Int -> State
run state 0 = state
run state amt = run (updateState state) (amt - 1)

updateState :: State -> State
updateState (zero, one, two, three, four, five, six, seven, eight) = (one, two, three, four, five, six, zero + seven, eight, zero)

total :: State -> Int
total (zero, one, two, three, four, five, six, seven, eight) = zero + one + two + three + four + five + six + seven + eight

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print (total (run (parse (head (lines contents)) (0, 0, 0, 0, 0, 0, 0, 0, 0)) 256))