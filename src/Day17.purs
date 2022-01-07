module Day17 where

import Prelude
import Data.Array ((..), filter)
import Data.Foldable (maximum, minimum)
import Data.Int (toNumber, ceil, floor)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set as Set
import Geometry (Point(..), Rect, maxX, maxY, minX, minY, contains)

bestY :: Rect -> Int
bestY r =
  let
    topY = maxY r

    bottomY = minY r
  in
    if abs bottomY > abs topY then bottomY else topY

maxInitialY :: Rect -> Int
maxInitialY r =
  let
    best = bestY r
  in
    if best >= 0 then best else abs best # flip sub 1

sumTo :: Int -> Int
sumTo n
  | n == 0 = 0
  | otherwise = n + sumTo (n - 1)

sumToUntilX :: Int -> Int -> Int
sumToUntilX n times
  | n == 0 = 0
  | times == 0 = 0
  | otherwise = n + sumToUntilX (n - 1) (times - 1)

sumToUntilY :: Int -> Int -> Int
sumToUntilY n times
  | times == 0 = 0
  | otherwise = n + sumToUntilY (n - 1) (times - 1)

sumToScan :: Int -> Array Int
sumToScan n
  | n == 0 = [ ]
  | otherwise = [ n ] <> (sumToScan (n - 1) <#> add n)

sumToScanUntil :: Int -> Int -> Array Int
sumToScanUntil n times
  | n == 0 = [ ]
  | times == 1 = [ n ]
  | otherwise = [ n ] <> (sumToScanUntil (n - 1) (times - 1) <#> add n)

highestY :: Int -> Int
highestY = sumTo

solve1 :: Rect -> Int
solve1 = maxInitialY >>> highestY

maxStepsY :: Int -> Rect -> Int
maxStepsY initialY =
  bestY
    >>> case _ of
        n
          | n > 0 -> 2 * initialY
          | n == 0 -> 2 * initialY + 1
          | otherwise -> 2 * initialY + 2

initV :: Int -> Int -> Number
initV v steps = (toNumber (v + (sumTo (steps - 1)))) / (toNumber steps)

maxStepsX :: Rect -> Int
maxStepsX r = checkStep 1
  where
  checkStep s
    | (floor $ flip initV s $ maxX r) > s = checkStep (s + 1)
    | otherwise = s

initRange :: Array Int -> Int -> Maybe (Array Int)
initRange targets steps = do
  minV <- minimum targets
  maxV <- maximum targets
  minInit <- pure $ ceil $ initV minV steps
  maxInit <- pure $ floor $ initV maxV steps
  pure $ minInit .. maxInit

initXRange :: Rect -> Int -> Maybe (Array Int)
initXRange target = initRange ((minX target) .. (maxX target))

initYRange :: Rect -> Int -> Maybe (Array Int)
initYRange target = initRange ((minY target) .. (maxY target))

makePoints :: Array Int -> Array Int -> Array Point
makePoints xs ys = do
  x <- xs
  y <- ys
  pure $ Point { x, y }

stepPoints :: Int -> Rect -> Maybe (Array Point)
stepPoints step target = do
  maxXSteps <- pure $ maxStepsX target
  maxYSteps <- pure $ flip maxStepsY target $ maxInitialY target
  xStep <- pure $ min maxXSteps step
  yStep <- pure $ min maxYSteps step
  if xStep < step && yStep < step then
    Nothing
  else do
    xs <- initXRange target xStep
    ys <- initYRange target yStep
    pure $ makePoints xs ys

uniqueInitPoints :: Rect -> Set Point
uniqueInitPoints target = collectInitPoints 1 Set.empty
  where
  collectInitPoints step set = case stepPoints step target of
    Just newPoints -> collectInitPoints (step + 1) ((Set.fromFoldable newPoints) <> set)
    Nothing -> set

uniqueInitPointsFiltered :: Rect -> Set Point
uniqueInitPointsFiltered target = collectInitPoints 1 Set.empty
  where
  collectInitPoints step set = case stepPoints step target of
    Just newPoints -> collectInitPoints (step + 1) ((Set.fromFoldable (filter (\p -> pointInTargetAtStep p step target) newPoints)) <> set)
    Nothing -> set

pointAtStep :: Point -> Int -> Point
pointAtStep (Point { x, y }) times = Point { x: sumToUntilX x times, y: sumToUntilY y times }

pointInTargetAtStep :: Point -> Int -> Rect -> Boolean
pointInTargetAtStep p times target = contains (pointAtStep p times) target

solve2 :: Rect -> Int
solve2 = uniqueInitPointsFiltered >>> Set.size
