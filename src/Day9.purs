module Day9 where

import Prelude

import Control.Monad.State (State, evalState, execState, get, modify_)
import Data.Array (all, catMaybes, concat, elem, filterA, mapWithIndex, modifyAt, sortBy, take)
import Data.Foldable (foldl, sum)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromJust)
import Data.Set (Set, fromFoldable, insert, member, singleton, size, toUnfoldable)
import Data.Traversable (traverse)
import Geometry (Grid, NeighborType(..), Point, gridFromIntStrings, gridPoints, gridValueAt, validNeighbors)
import Partial.Unsafe (unsafePartial)

type FloorMap
  = Grid Int

type Basin
  = Set Point

type StateData
  = { map :: FloorMap
    , lowPoints :: Array Point
    , basins :: Array Basin
    }

type FloorState
  = State StateData

isLowPoint :: Point -> FloorState Boolean
isLowPoint p = do
  { map } <- get
  mV <- pure $ gridValueAt p map
  mVals <- pure $ validNeighbors p Adjacent map # traverse \n-> gridValueAt n map
  pure
    $ case mV of
        Just v -> case mVals of
          Just vals -> all (\i -> i > v) vals
          Nothing -> false -- should be Nothing...
        Nothing -> false -- should be Nothing...

findLowPoints :: FloorState Unit
findLowPoints = do
  { map } <- get
  lows <- gridPoints map # filterA isLowPoint
  modify_ \s@{ lowPoints } -> s { lowPoints = concat [ lowPoints, lows ] }

riskLevel :: FloorState (Maybe Int)
riskLevel = do
  _ <- findLowPoints
  { lowPoints, map: m } <- get
  lowValues <- pure $ traverse (\p -> gridValueAt p m) lowPoints
  pure $ lowValues <#> map (add 1) <#> sum

basinIndexes :: Point -> FloorState (Array Int)
basinIndexes point = do
  { basins } <- get
  pure $ mapWithIndex (\i s -> if member point s then Just i else Nothing) basins # catMaybes

adjacentBasins :: Point -> FloorState (Array Int)
adjacentBasins point = do
  { map } <- get
  ns <- pure $ validNeighbors point Adjacent map
  traverse basinIndexes ns <#> concat

newBasin :: Point -> FloorState Unit
newBasin point = modify_ \s@{ basins } -> s { basins = append [ singleton point ] basins }

addToBasin :: Point -> Int -> FloorState Unit
addToBasin point index =
  modify_ \s@{ basins } ->
    s { basins = unsafePartial $ fromJust $ modifyAt index (insert point) basins }

updateBasins :: Point -> FloorState Unit
updateBasins p = do
  indicesToUpdate <- adjacentBasins p
  { basins } <- get
  { no, yes } <-
    pure
      $ foldlWithIndex
          ( \i a v ->
              if elem i indicesToUpdate then
                a { yes = append a.yes [ v ] }
              else
                a { no = append a.no [ v ] }
          )
          { no: [], yes: [] }
          basins
  merged <- pure $ yes <#> toUnfoldable # concat # fromFoldable # \s -> insert p s
  modify_ \s -> s { basins = concat [ no, [ merged ] ] }

checkBasin :: Point -> FloorState Unit
checkBasin p = do
  { map } <- get
  mVal <- pure $ gridValueAt p map
  case mVal of
    Nothing -> pure unit
    Just 9 -> pure unit
    Just _ -> updateBasins p

findBasins :: FloorState Unit
findBasins = do
  { map } <- get
  points <- pure $ gridPoints map
  _ <- traverse checkBasin points
  pure unit

solve1 :: String -> Maybe Int
solve1 input = do
  map <- gridFromIntStrings input
  evalState riskLevel { map, lowPoints: [], basins: [] }

solve2 :: String -> Maybe Int
solve2 input = do
  map <- gridFromIntStrings input
  { basins } <- pure $ execState findBasins { map, lowPoints: [], basins: [] }
  sorted <- pure $ sortBy (\b1 b2 -> compare (size b2) (size b1)) basins
  pure $ foldl mul 1 $ (take 3 sorted) <#> size
