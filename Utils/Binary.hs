module Utils.Binary (module Utils.Binary) where

binaryFlip :: String -> String
binaryFlip = map flip
    where flip x = if x == '0' then '1' else '0'

binaryToNumber :: String -> Int
binaryToNumber = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1