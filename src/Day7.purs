module Day7 where

import Prelude
import Data.Array ((..))
import Data.Foldable (foldl, sum, minimum, maximum)
import Data.Maybe (Maybe)
import Data.Ord (abs)

distanceFrom :: Int -> Array Int -> Int
distanceFrom med = foldl (\a v -> a + (abs (v - med))) 0

distanceFrom2 :: Int -> Array Int -> Int
distanceFrom2 pt = foldl (\a v -> a + (sum ((abs (v - pt)) .. 0))) 0

solve2 :: Array Int -> Maybe Int
solve2 input = do
  minValue <- minimum input
  maxValue <- maximum input
  all <- pure $ (minValue .. maxValue) <#> \pt -> distanceFrom2 pt input
  minimum all
