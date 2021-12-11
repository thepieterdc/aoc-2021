module Utils.Filtering (module Utils.Filtering) where

import Data.List

filterByLength :: Int -> [String] -> [String]
filterByLength len = filter (\d -> length d == len)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

filterContainsSubString :: String -> [String] -> [String]
filterContainsSubString needle = filter (\d -> length (d `intersect` needle) == length needle)

howMany :: (a -> Bool) -> [a] -> Int
howMany f items = length (filter f items)