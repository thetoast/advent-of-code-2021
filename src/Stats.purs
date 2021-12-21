module Stats where

import Prelude
import Data.Array (length, sort, (!!))
import Data.Maybe (Maybe(..))

avg :: Int -> Int -> Int
avg a b = (a + b) / 2

avgN :: Number -> Number -> Number
avgN a b = (a + b) / 2.0

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

medianN :: Array Number -> Maybe Number
medianN arr
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
        pure $ avgN a b
  | otherwise = Nothing

