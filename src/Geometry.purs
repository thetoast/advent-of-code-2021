module Geometry where

import Prelude
import Data.Array (concat, filter, intercalate, length, modifyAt, replicate, updateAt, (!!), (..))
import Data.Array.NonEmpty ((!!)) as NE
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..))
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------
newtype Point
  = Point
  { x :: Int
  , y :: Int
  }

instance eqPoint :: Eq Point where
  eq (Point p1) (Point p2) = p1.x == p2.x && p1.y == p2.y

instance ordPoint :: Ord Point where
  compare (Point p1) (Point p2) = if p1.x == p2.x then compare p1.y p2.y else compare p1.x p2.x

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> show x <> "," <> show y <> ")"

pointRegex :: Regex
pointRegex = unsafeRegex "\\(?(\\d+),(\\d+)\\)?" noFlags

pointFromString :: String -> Maybe Point
pointFromString s = do
  m <- match pointRegex s
  x <- fromString =<< (join $ m NE.!! 1)
  y <- fromString =<< (join $ m NE.!! 2)
  pure $ Point { x, y }

--------------------------------------------------------------------------------
-- Line
--------------------------------------------------------------------------------
newtype Line
  = Line
  { start :: Point
  , stop :: Point
  }

instance showLine :: Show Line where
  show (Line { start, stop }) = show start <> " -> " <> show stop

--------------------------------------------------------------------------------
-- Dimensions
--------------------------------------------------------------------------------
newtype Dimensions
  = Dimensions
  { width :: Int
  , height :: Int
  }

instance showDimensions :: Show Dimensions where
  show (Dimensions { width, height }) = show width <> "x" <> show height

--------------------------------------------------------------------------------
-- Rect
--------------------------------------------------------------------------------
newtype Rect
  = Rect
  { origin :: Point
  , size :: Dimensions
  }

instance showRect :: Show Rect where
  show (Rect { origin, size }) = "( " <> show origin <> " " <> show size <> " )"

--------------------------------------------------------------------------------
-- Grid
--------------------------------------------------------------------------------
newtype Grid a
  = Grid (Array (Array a))

instance showGridStrings :: Show (Grid String) where
  show (Grid rows) =
    let
      showRow = foldl (<>) "\n"
    in
      map showRow rows # foldl (<>) ""
else instance showGrid :: Show a => Show (Grid a) where
  show (Grid rows) =
    let
      showRow = map show >>> intercalate "" >>> (<>) "\n"
    in
      map showRow rows # foldl (<>) ""

data NeighborType
  = Adjacent
  | Diagonal

neighbors :: Point -> NeighborType -> Array Point
neighbors (Point { x, y }) Adjacent =
  [ Point { x: x - 1, y }
  , Point { x: x + 1, y }
  , Point { x, y: y + 1 }
  , Point { x, y: y - 1 }
  ]

neighbors p@(Point { x, y }) Diagonal =
  concat
    [ neighbors p Adjacent
    , [ Point { x: x - 1, y: y - 1 }
      , Point { x: x + 1, y: y - 1 }
      , Point { x: x - 1, y: y + 1 }
      , Point { x: x + 1, y: y + 1 }
      ]
    ]

inGrid :: forall a. Grid a -> Point -> Boolean
inGrid grid (Point { x, y }) = case gridDimensions grid of
  Just (Dimensions d) -> x >= 0 && y >= 0 && x <= d.width - 1 && y <= d.height - 1
  Nothing -> false

validNeighbors :: forall a. Point -> NeighborType -> Grid a -> Array Point
validNeighbors point ntype g = neighbors point ntype # filter (inGrid g)

gridPoints :: forall a. Grid a -> Array Point
gridPoints g = case gridDimensions g of
  Just (Dimensions d) -> 0 .. (d.height - 1) >>= \y -> 0 .. (d.width - 1) <#> \x -> Point { x, y }
  Nothing -> []

gridDimensions :: forall a. Grid a -> Maybe Dimensions
gridDimensions (Grid rows) = do
  height <- pure $ length rows
  width <- length <$> rows !! 0
  pure $ Dimensions { width, height }

gridValueAt :: forall a. Point -> Grid a -> Maybe a
gridValueAt (Point { x, y }) (Grid ys) = ys !! y >>= \xs -> xs !! x

toGrid :: forall a. Array (Array a) -> Maybe (Grid a)
toGrid a = Just (Grid a)

parseLine :: String -> Maybe (Array Int)
parseLine = split (Pattern "") >>> traverse fromString

gridFromIntStrings :: String -> Maybe (Grid Int)
gridFromIntStrings = split (Pattern "\n") >>> traverse parseLine >=> toGrid

modifyGridAt :: forall a. Point -> (a -> a) -> Grid a -> Maybe (Grid a)
modifyGridAt (Point { x, y }) fn (Grid rows) = do
  cols <- rows !! y
  newRow <- modifyAt x fn cols
  updateAt y newRow rows >>= toGrid

insertGridAt :: forall a. Point -> a -> Grid a -> Maybe (Grid a)
insertGridAt p i = modifyGridAt p (\_ -> i)

-- | does not handle negative x/y coords, assumes 0,0 origin
gridFromPoints :: forall a. Array Point -> Tuple a a -> Maybe (Grid a)
gridFromPoints points (Tuple emptyValue filledValue) = do
  (Point { x: x0, y: y0 }) <- points !! 0
  { maxX, maxY } <- pure $ foldl updateMax { maxX: x0, maxY: y0 } points
  grid <- pure $ Grid $ (\n -> replicate n emptyValue) <$> replicate (maxY + 1) (maxX + 1)
  foldl (\g p -> g >>= insertGridAt p filledValue) (Just grid) points
  where
  updateMax a (Point { x, y }) =
    { maxX: max x a.maxX
    , maxY: max y a.maxY
    }
