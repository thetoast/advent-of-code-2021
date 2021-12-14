module Day7 where

import Prelude
import Data.Array ((..), sort, length, (!!))
import Data.Foldable (foldl, sum, minimum, maximum)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)

avg :: Int -> Int -> Int
avg a b = (a + b) / 2

median :: Array Int -> Maybe Int
median arr
  | length arr == 1 = arr !! 0
  | length arr `mod` 2 == 1 = sort arr !! (((length arr) - 1) / 2)
  | length arr `mod` 2 == 0 =
    let
      mid = (((length arr) / 2) - 1)
      sorted = sort arr
    in
      do
        a <- sorted !! mid
        b <- sorted !! (mid + 1)
        pure $ avg a b
  | otherwise = Nothing

distanceFrom :: Int -> Array Int -> Int
distanceFrom med = foldl (\a v -> a + (abs (v - med))) 0

distanceFrom2 :: Int -> Array Int -> Int
distanceFrom2 pt = foldl (\a v -> a + (sum ((abs (v - pt)) .. 0))) 0

solve2 :: Array Int -> Maybe Int
solve2 input =
  do
    minValue <- minimum input
    maxValue <- maximum input
    all <- pure $ (minValue .. maxValue) <#> \pt -> distanceFrom2 pt input
    minimum all
