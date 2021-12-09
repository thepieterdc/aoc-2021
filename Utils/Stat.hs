module Utils.Stat (module Utils.Stat) where

average :: [Double] -> Double
average inputs = sum inputs / fromIntegral (length inputs)

median :: [a] -> (a, a)
median [x] = (x, x)
median [x, y] = (x, y)
median (x:y:xs) = median (tail (reverse xs))

naturalSum :: Int -> Int
naturalSum n = round (fromIntegral (n * (n + 1)) / 2)