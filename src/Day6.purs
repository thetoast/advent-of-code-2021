module Day6 where

import Prelude

import Data.Foldable (foldl, sum)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map, empty, insert, insertWith, values)

type Timer = Int
type TimerBucket = Map Timer Number

makeBucket :: Array Int -> TimerBucket
makeBucket = foldl (\m t -> insertWith (+) t 1.0 m) empty

tickBucket :: TimerBucket -> TimerBucket
tickBucket = foldlWithIndex tick empty
  where
  tick k m v | k == 0 = insertWith (+) 6 v m # insert 8 v
             | otherwise = insertWith (+) (k-1) v m

runBucketDays :: Int -> TimerBucket -> TimerBucket
runBucketDays num bucket
  | num == 0 = bucket
  | otherwise = runBucketDays (num - 1) (tickBucket bucket)

solve1 :: Int -> Array Int -> Number
solve1 days timers = makeBucket timers # runBucketDays days # values # sum
