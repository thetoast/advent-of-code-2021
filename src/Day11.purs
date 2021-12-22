module Day11 where

import Prelude
import Data.Array (filter, filterA, length, (..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty, insert, member, size, toUnfoldable)
import Geometry (Grid, NeighborType(..), Point, gridFromIntStrings, gridPoints, gridValueAt, insertGridAt, modifyGridAt, validNeighbors)

type Octopus
  = Int

type Octopi
  = Grid Octopus

parseInput :: String -> Maybe Octopi
parseInput = gridFromIntStrings

shouldFlash :: Point -> Octopi -> Maybe Boolean
shouldFlash p o = gridValueAt p o <#> \v -> v > 9

type FlashResult
  = { map :: Octopi
    , haveFlashed :: Set Point
    }

bump :: Maybe Octopi -> Point -> Maybe Octopi
bump m p = m >>= modifyGridAt p (add 1)

reset :: Maybe Octopi -> Point -> Maybe Octopi
reset m p = m >>= insertGridAt p 0

flash :: Maybe FlashResult -> Point -> Maybe FlashResult
flash mr p =
  mr
    >>= \r ->
        let
          newMap = foldl bump (Just r.map) $ validNeighbors p Diagonal r.map
        in
          newMap
            <#> \new ->
                { map: new
                , haveFlashed: insert p r.haveFlashed
                }

flashPoints :: FlashResult -> Array Point -> Maybe FlashResult
flashPoints r ps = foldl flash (Just r) ps

flashRec :: FlashResult -> Array Point -> Maybe FlashResult
flashRec r ps = do
  points <- pure $ gridPoints r.map
  newState <- flashPoints r ps
  toFlash <- filterA (\n -> shouldFlash n newState.map) $ filter (\n -> not $ member n newState.haveFlashed) $ points
  if (length toFlash) > 0 then flashRec newState toFlash else pure newState

type StepResult
  = { map :: Octopi
    , flashed :: Int
    }

step :: StepResult -> Maybe StepResult
step r = do
  points <- pure $ gridPoints r.map
  newGrid <- foldl bump (Just r.map) points
  toFlash <- filterA (\p -> shouldFlash p newGrid) points
  postFlash <- flashRec { map: newGrid, haveFlashed: empty } toFlash
  finalMap <- foldl reset (Just postFlash.map) (toUnfoldable postFlash.haveFlashed :: Array Point)
  pure r { map = finalMap, flashed = r.flashed + size postFlash.haveFlashed }

solveSteps :: String -> Int -> Maybe StepResult
solveSteps input steps = do
  map <- parseInput input
  foldl (\r _ -> r >>= step) (Just { map, flashed: 0 }) (0 .. (steps - 1))

findSyncRec :: StepResult -> Int -> Maybe Int
findSyncRec r i = do
  maxCount <- pure $ length $ gridPoints r.map
  { map, flashed } <- step r
  if flashed == maxCount then pure i else findSyncRec { map, flashed: 0 } (i + 1)

solveSync :: String -> Maybe Int
solveSync input = do
  map <- parseInput input
  findSyncRec { map, flashed: 0 } 1
