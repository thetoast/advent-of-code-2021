module Day5 where

import Prelude
import Data.Array ((!!), filter)
import Data.Array.NonEmpty (index)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.List (filter, length) as List
import Data.Map (values)
import Data.Maybe (Maybe, fromJust)
import Data.String (length, split, Pattern(..))
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)

import Day5Input (testInputRaw, realInputRaw)
import Geometry (Point(..), Line(..), Dimensions(..), Rect(..))
import OceanFloor (OceanFloor(..), makeFloor, markLine, markLine2)

linePattern :: Regex
linePattern = unsafeRegex "(.*),(.*) -> (.*),(.*)" noFlags

parseLine :: String -> Maybe Line
parseLine string = do
  m <- match linePattern string
  x1 <- index m 1 # join >>= fromString
  y1 <- index m 2 # join >>= fromString
  x2 <- index m 3 # join >>= fromString
  y2 <- index m 4 # join >>= fromString
  pure $ Line { start: Point { x: x1, y: y1 }, stop: Point { x: x2, y: y2 } }

makeInput :: String -> Maybe (Array Line)
makeInput = split (Pattern "\n") >>> filter (length >>> (_ > 0)) >>> traverse parseLine

testInput :: Maybe (Array Line)
testInput = makeInput testInputRaw

realInput :: Maybe (Array Line)
realInput = makeInput realInputRaw

lineBounds :: Array Line -> Rect
lineBounds lines = foldl checkBounds initBounds lines # makeBounds
  where
  Line { start: Point start, stop: Point stop } = unsafePartial $ fromJust $ lines !! 0

  initBounds =
    { minX: min start.x stop.x
    , maxX: max start.x stop.x
    , minY: min start.y stop.y
    , maxY: max start.y stop.y
    }

  checkBounds bounds (Line { start: Point start, stop: Point stop }) =
    bounds
      { minX = min bounds.minX (min start.x stop.x)
      , maxX = max bounds.maxX (max start.x stop.x)
      , minY = min bounds.minY (min start.y stop.y)
      , maxY = max bounds.maxY (max start.y stop.y)
      }

  makeBounds bounds =
    Rect
      { origin: Point { x: bounds.minX, y: bounds.minY }
      , size:
          Dimensions
            { width: bounds.maxX - bounds.minX
            , height: bounds.maxY - bounds.minY
            }
      }

solve1 :: String -> Maybe Int
solve1 rawLines = do
  lines <- makeInput rawLines
  floor <- pure $ makeFloor $ lineBounds lines
  OceanFloor { map } <- pure $ foldl markLine floor lines
  pure $ values map # List.filter (\v -> v > 1) # List.length

solve2 :: String -> Maybe Int
solve2 rawLines = do
  lines <- makeInput rawLines
  floor <- pure $ makeFloor $ lineBounds lines
  OceanFloor { map } <- pure $ foldl markLine2 floor lines
  pure $ values map # List.filter (\v -> v > 1) # List.length
