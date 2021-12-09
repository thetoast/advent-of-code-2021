module BingoBoard where

import Prelude
import Data.Array ((..), (!!), length, elemIndex, modifyAt, filter)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)

type BingoSpace
  = { marked :: Boolean
    , number :: Int
    }

type BingoRow
  = Array BingoSpace

newtype BingoBoard
  = BingoBoard
  { spaces :: Array BingoSpace
  , rowIndices :: Array (Array Int)
  , colIndices :: Array (Array Int)
  }

instance showBoard :: Show BingoBoard where
  show (BingoBoard board) = foldl (<>) "" markedRows
    where
    padShow i
      | i < 10 = " " <> show i
      | otherwise = show i

    markedRows :: Array String
    markedRows = board.rowIndices <#> markedRow

    markedRow :: Array Int -> String
    markedRow row =
      (<>) "\n"
        $ foldl (<>) ""
        $ row
        <#> \index ->
            let
              space = unsafePartial $ fromJust $ board.spaces !! index
            in
              if space.marked then
                "(" <> padShow space.number <> ") "
              else
                " " <> padShow space.number <> "  "

makeRow :: Array Int -> Maybe BingoRow
makeRow numbers
  | length numbers == 5 = numbers <#> (\i -> { marked: false, number: i }) # Just
  | otherwise = Nothing

rowIndices :: Array (Array Int)
rowIndices = 0 .. 4 <#> (\x -> 0 .. 4 <#> \y -> x * 5 + y)

colIndices :: Array (Array Int)
colIndices = 0 .. 4 <#> (\x -> 0 .. 4 <#> \y -> y * 5 + x)

makeBoard :: Array (Array Int) -> Maybe BingoBoard
makeBoard rows
  | length rows == 5 = do
    boardRows <- traverse makeRow rows
    spaces <- pure $ join boardRows
    Just (BingoBoard { spaces, rowIndices, colIndices })
  | otherwise = Nothing

markBoard :: Int -> BingoBoard -> Maybe BingoBoard
markBoard number (BingoBoard board) = do
  index <- elemIndex { marked: false, number } board.spaces
  newSpaces <- modifyAt index (\s -> s { marked = true }) board.spaces
  pure $ BingoBoard board { spaces = newSpaces }

hasWon :: BingoBoard -> Boolean
hasWon (BingoBoard board) = hasCompletedRow || hasCompletedCol
  where
  isMarked space = space.marked == true

  lineCompleted = filter isMarked >>> length >>> eq 5

  getSpace i = unsafePartial $ fromJust $ board.spaces !! i

  rows = board.rowIndices <#> \r -> r <#> getSpace

  cols = board.colIndices <#> \r -> r <#> getSpace

  hasCompleted = filter lineCompleted >>> length >>> \l -> l >= 1

  hasCompletedRow = hasCompleted rows

  hasCompletedCol = hasCompleted cols
