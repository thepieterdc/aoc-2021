module Utils.Geometry.Line (module Utils.Geometry.Line) where

import Utils.Geometry.Base (Coordinate)

type Line = (Coordinate, Coordinate)

generateDiagonal :: Coordinate -> Coordinate -> [Coordinate]
generateDiagonal (x1, y1) (x2, y2)
    | x1 == x2 = [(x2, y2)]
    | x1 < x2 && y1 < y2 = (x1, y1) : generateDiagonal (x1 + 1, y1 + 1) (x2, y2)
    | x1 < x2 && y1 > y2 = (x1, y1) : generateDiagonal (x1 + 1, y1 - 1) (x2, y2)
    | x1 > x2 && y1 < y2 = (x1, y1) : generateDiagonal (x1 - 1, y1 + 1) (x2, y2)
    | otherwise = (x1, y1) : generateDiagonal (x1 - 1, y1 - 1) (x2, y2)

generateHorizontal :: Coordinate -> Coordinate -> [Coordinate]
generateHorizontal (x1, y) (x2, _) = [(x', y) | x' <- [min..max]]
    where (min, max) = (minimum [x1, x2], maximum [x1, x2])

generateVertical :: Coordinate -> Coordinate -> [Coordinate]
generateVertical (x, y1) (_, y2) = [(x, y') | y' <- [min..max]]
    where (min, max) = (minimum [y1, y2], maximum [y1, y2])

getCoordinates :: Line -> [Coordinate]
getCoordinates ((x1, y1), (x2, y2)) = case (x1 == x2, y1 == y2) of
    (True, _) -> generateVertical (x1, y1) (x2, y2)
    (_, True) -> generateHorizontal (x1, y1) (x2, y2)
    _ -> generateDiagonal (x1, y1) (x2, y2)

isDiagonal :: Line -> Bool
isDiagonal ((x1, y1), (x2, y2)) = (x1 /= x2) && (y1 /= y2)