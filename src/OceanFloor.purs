module OceanFloor where

import Prelude
import Data.Array ((..), zip)
import Data.Foldable (foldl)
import Data.Map (Map, empty, insertWith, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Geometry (Dimensions(..), Point(..), Rect(..), Line(..))

newtype OceanFloor
  = OceanFloor
  { map :: Map (Tuple Int Int) Int
  , geo :: Rect
  }

instance showOceanFloor :: Show OceanFloor where
  show ( OceanFloor
      { map
    , geo:
        Rect
          { origin: Point { x, y }
        , size: Dimensions { width, height }
        }
    }
  ) = ys <#> printRow # foldl (<>) ""
    where
    xs = x .. (x + width)

    ys = y .. (y + height)

    printRow y' = xs <#> (\x'' -> printPoint x'' y') # foldl (<>) "" # (<>) "\n"

    printPoint x' y' = case lookup (Tuple x' y') map of
      Just i -> show i
      Nothing -> "."

makeFloor :: Rect -> OceanFloor
makeFloor geo = OceanFloor { map: empty, geo }

addPoints :: OceanFloor -> Array (Tuple Int Int) -> OceanFloor
addPoints (OceanFloor floor@{ map }) points = foldl addPoint map points # \newMap -> OceanFloor floor { map = newMap }
  where
  addPoint mapAcc point = insertWith (+) point 1 mapAcc

markLine :: OceanFloor -> Line -> OceanFloor
markLine floor (Line { start: (Point { x: x1, y: y1 }), stop: (Point { x: x2, y: y2 }) }) =
  let
    points =
      if x1 == x2 then
        y1 .. y2 <#> Tuple x1
      else if y1 == y2 then
        x1 .. x2 <#> \x -> Tuple x y1
      else
        []
  in
    addPoints floor points

markLine2 :: OceanFloor -> Line -> OceanFloor
markLine2 floor (Line { start: (Point { x: x1, y: y1 }), stop: (Point { x: x2, y: y2 }) }) =
  let
    points =
      if x1 == x2 then
        y1 .. y2 <#> Tuple x1
      else if y1 == y2 then
        x1 .. x2 <#> \x -> Tuple x y1
      else
        zip (x1 .. x2) (y1 .. y2)
  in
    addPoints floor points
