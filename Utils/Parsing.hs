module Utils.Parsing (module Utils.Parsing) where

parseInt :: String -> Int
parseInt a = read a :: Int

parseCharToInt :: Char -> Int
parseCharToInt a = read [a] :: Int