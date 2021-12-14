module Utils.Counter (module Utils.Counter) where

import Data.List
import Data.Map (Map)
import Data.Ord
import qualified Data.Map.Strict as Map

type Counter k = Map k Int

increase :: (Ord k) => k -> Int -> Counter k -> Counter k
increase = Map.insertWith (+)

increment :: (Ord k) => k -> Counter k -> Counter k
increment k = increase k 1

maxEntry :: (Ord k) => Counter k -> (k, Int)
maxEntry counter = maximumBy (comparing snd) (Map.assocs counter)

maxValue :: (Ord k) => Counter k -> Int
maxValue counter = maximum (Map.elems counter)

minEntry :: (Ord k) => Counter k -> (k, Int)
minEntry counter = minimumBy (comparing snd) (Map.assocs counter)

minValue :: (Ord k) => Counter k -> Int
minValue counter = minimum (Map.elems counter)