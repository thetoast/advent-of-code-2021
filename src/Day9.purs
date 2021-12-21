module Day9 where

import Prelude
import Control.Monad.State (State, evalState, execState, get, gets, modify_)
import Data.Array (all, catMaybes, concat, elem, filterA, length, mapWithIndex, modifyAt, sortBy, take, (!!), (..))
import Data.Foldable (foldl, sum)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.Set (Set, fromFoldable, insert, member, singleton, size, toUnfoldable)
import Data.String (Pattern(..), split)
import Data.Traversable (sequence, traverse)
import Geometry (Dimensions(..), Point(..))
import Partial.Unsafe (unsafePartial)

type FloorMap
  = Array (Array Int)

type Basin
  = Set Point

type StateData
  = { map :: FloorMap
    , dimensions :: Dimensions
    , lowPoints :: Array Point
    , basins :: Array Basin
    }

type FloorState
  = State StateData

parseLine :: String -> Maybe (Array Int)
parseLine = split (Pattern "") >>> traverse fromString

parseInput :: String -> Maybe FloorMap
parseInput = split (Pattern "\n") >>> traverse parseLine

dimensions :: FloorMap -> Maybe Dimensions
dimensions floor = do
  height <- pure $ length floor
  width <- length <$> floor !! 0
  pure $ Dimensions { width, height }

neighbors :: Point -> Array Point
neighbors (Point { x, y }) =
  [ Point { x: x - 1, y }
  , Point { x: x + 1, y }
  , Point { x, y: y + 1 }
  , Point { x, y: y - 1 }
  ]

inBounds :: Point -> FloorState Boolean
inBounds (Point { x, y }) =
  gets \{ dimensions: Dimensions d } ->
    x >= 0 && y >= 0 && x <= d.width - 1 && y <= d.height - 1

validNeighbors :: Point -> FloorState (Array Point)
validNeighbors = neighbors >>> filterA inBounds

valueAt :: Point -> FloorState (Maybe Int)
valueAt (Point { x, y }) = gets \{ map } -> map !! y >>= \xs -> xs !! x

isLowPoint :: Point -> FloorState Boolean
isLowPoint p = do
  mV <- valueAt p
  mVals <- validNeighbors p >>= traverse valueAt <#> sequence
  pure
    $ case mV of
        Just v -> case mVals of
          Just vals -> all (\i -> i > v) vals
          Nothing -> false -- should be Nothing...
        Nothing -> false -- should be Nothing...

allPoints :: FloorState (Array Point)
allPoints =
  gets \{ dimensions: Dimensions { width, height } } ->
    0 .. (height - 1) >>= \y -> 0 .. (width - 1) <#> \x -> Point { x, y }

findLowPoints :: FloorState Unit
findLowPoints = do
  lows <- allPoints >>= filterA isLowPoint
  modify_ \s@{ lowPoints } -> s { lowPoints = concat [ lowPoints, lows ] }

riskLevel :: FloorState (Maybe Int)
riskLevel = do
  _ <- findLowPoints
  { lowPoints } <- get
  lowValues <- traverse valueAt lowPoints <#> sequence
  pure $ lowValues <#> map (add 1) <#> sum

basinIndexes :: Point -> FloorState (Array Int)
basinIndexes point = do
  { basins } <- get
  pure $ mapWithIndex (\i s -> if member point s then Just i else Nothing) basins # catMaybes

adjacentBasins :: Point -> FloorState (Array Int)
adjacentBasins point = do
  ns <- validNeighbors point
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
  mVal <- valueAt p
  case mVal of
    Nothing -> pure unit
    Just 9 -> pure unit
    Just _ -> updateBasins p

findBasins :: FloorState Unit
findBasins = do
  points <- allPoints
  _ <- traverse checkBasin points
  pure unit

solve1 :: String -> Maybe Int
solve1 input = do
  map <- parseInput input
  ds <- dimensions map
  evalState riskLevel { map, dimensions: ds, lowPoints: [], basins: [] }

solve2 :: String -> Maybe Int
solve2 input = do
  map <- parseInput input
  ds <- dimensions map
  { basins } <- pure $ execState findBasins { map, dimensions: ds, lowPoints: [], basins: [] }
  sorted <- pure $ sortBy (\b1 b2 -> compare (size b2) (size b1)) basins
  pure $ foldl mul 1 $ (take 3 sorted) <#> size
