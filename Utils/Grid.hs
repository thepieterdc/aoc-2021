module Utils.Grid (module Utils.Grid) where

import Data.List

type Coordinate = (Int, Int)

type Grid t = [Row t]
type Row t = [t]

floodFill :: (Coordinate -> Bool) -> Grid a -> [Coordinate] -> [Coordinate] -> [Coordinate]
floodFill _ _ [] seen = []
floodFill edge grid (x:todo) seen = if edge x
    then floodFill edge grid todo seen
    else x : floodFill edge grid (todo `union` (getOrthogonalNeighbours grid x \\ seen)) (x : seen) 

getCoordinates :: Grid a -> [Coordinate]
getCoordinates grid = [(r, c) | r <- [0..length grid - 1], c <- [0..length (head grid) - 1]]

-- getOrthogonalNeighbours: gets the vertical and horizontal neighbours.
getOrthogonalNeighbours :: Grid a -> Coordinate -> [Coordinate]
getOrthogonalNeighbours grid (r, c) = case (r, c, r + 1 == length grid, c + 1 == length (head grid)) of
    -- 4 corners.
    (0, 0, _, _) -> [(0, 1), (1, 0)]
    (_, _, True, True) -> [(r, c - 1), (r - 1, c)]
    (_, 0, True, _) -> [(r, 1), (r - 1, 0)]
    (0, _, _, True) -> [(0, c - 1), (1, c)]
    -- 4 edges.
    (0, _, _, _) -> [(0, c - 1), (0, c + 1), (1, c)]
    (_, _, _, True) -> [(r - 1, c), (r + 1, c), (r, c - 1)]
    (_, _, True, _) -> [(r, c - 1), (r, c + 1), (r - 1, c)]
    (_, 0, _, _) -> [(r - 1, c), (r + 1, c), (r, c + 1)]
    -- Everywhere else.
    (_, _, _, _) -> [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

value :: Grid a -> Coordinate -> a
value grid (r, c) = (grid !! r) !! c