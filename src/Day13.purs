module Day13 where

import Prelude
import Data.Array (filterA, mapMaybe, slice, (!!))
import Data.Array.NonEmpty ((!!)) as NE
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (split, Pattern(..))
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Geometry (Grid(..), Point(..), gridFromPoints, gridPoints, gridValueAt, insertGridAt, pointFromString)

type Paper
  = Grid String

type Fold
  = Tuple String Int

parsePoints :: String -> Maybe (Array Point)
parsePoints input = traverse pointFromString $ split (Pattern "\n") input

parseInput :: String -> Maybe Paper
parseInput input = parsePoints input >>= \p -> gridFromPoints p (Tuple "." "#")

foldRegex :: Regex
foldRegex = unsafeRegex "fold along ([xy])=(\\d+)" noFlags

foldFromString :: String -> Maybe Fold
foldFromString string = do
  m <- match foldRegex string
  axis <- join $ m NE.!! 1
  pos <- fromString =<< (join $ m NE.!! 2)
  pure $ Tuple axis pos

parseFolds :: String -> Maybe (Array Fold)
parseFolds input = traverse foldFromString $ split (Pattern "\n") input

fixCoords :: Fold -> Point -> Maybe Point
fixCoords (Tuple dir coord) (Point { x, y })
  | dir == "x" && x > coord = Just $ Point { x: coord - (abs (x - coord)), y }
  | dir == "y" && y > coord = Just $ Point { y: coord - (abs (y - coord)), x }
  | otherwise = Nothing

truncate :: Paper -> Fold -> Maybe Paper
truncate (Grid rows) (Tuple "x" coord) = Just $ Grid $ rows <#> slice 0 coord

truncate (Grid rows) (Tuple "y" coord) = Just $ Grid $ slice 0 coord rows

truncate _ _ = Nothing

foldPaper :: Paper -> Fold -> Maybe Paper
foldPaper paper fold = do
  points <- pure $ gridPoints paper
  updatePoints <- mapMaybe (fixCoords fold) <$> filterA (\p -> gridValueAt p paper <#> eq "#") points
  updatedGrid <- foldl (\g p -> g >>= insertGridAt p "#") (Just paper) updatePoints
  truncate updatedGrid fold

solve1 :: String -> String -> Maybe Int
solve1 points foldstr = do
  paper <- parseInput points
  folds <- parseFolds foldstr
  foldedPaper <- folds !! 0 >>= \f -> foldPaper paper f
  pure
    $ foldl
        ( \a p -> case gridValueAt p foldedPaper of
            Just v -> if v == "#" then a + 1 else a
            Nothing -> a
        )
        0
    $ gridPoints foldedPaper

solve2 :: String -> String -> Maybe Paper
solve2 points foldstr = do
  paper <- parseInput points
  folds <- parseFolds foldstr
  foldl (\p f -> p >>= \p' -> foldPaper p' f) (Just paper) folds
