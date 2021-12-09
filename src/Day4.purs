module Day4 where

import Prelude
import Data.Array (find, uncons, filter)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import BingoBoard (BingoBoard(..), makeBoard, markBoard, hasWon)
import Day4Input (inputNumbers, inputBoards, testNumbers, testBoards)

type RawInput = Array (Array (Array Int))

testBoard :: Array (Array Int)
testBoard =
  [ [ 1, 2, 3, 4, 5 ]
  , [ 6, 7, 8, 9, 10 ]
  , [ 11, 12, 13, 14, 15 ]
  , [ 16, 17, 18, 19, 20 ]
  , [ 21, 22, 23, 24, 25 ]
  ]

makeBoards :: RawInput -> Maybe (Array BingoBoard)
makeBoards boards = traverse makeBoard boards

markBoards :: Array BingoBoard -> Int -> Array BingoBoard
markBoards boards number = boards <#> markOrKeep
  where
  markOrKeep board = case markBoard number board of
    Just newBoard -> newBoard
    Nothing -> board

markAndTest :: Array BingoBoard -> Int -> Tuple (Array BingoBoard) (Maybe BingoBoard)
markAndTest boards number = Tuple (filter (\b -> hasWon b == false) newBoards) winner
  where
  newBoards = markBoards boards number

  winner = find hasWon newBoards

findWinner :: Array BingoBoard -> Array Int -> Maybe (Tuple BingoBoard Int)
findWinner boards numbers = case uncons numbers of
  Just { head: number, tail: rest } ->
    let
      (Tuple newBoards winner) = markAndTest boards number
    in
      case winner of
        Just winningBoard -> Just (Tuple winningBoard number)
        Nothing -> findWinner newBoards rest
  Nothing -> Nothing

findWinner2 :: Array BingoBoard -> Array Int -> Maybe (Tuple BingoBoard Int) -> Maybe (Tuple BingoBoard Int)
findWinner2 boards numbers lastWinner = case uncons numbers of
  Just { head: number, tail: rest } ->
    let
      (Tuple newBoards winner) = markAndTest boards number
    in
      case winner of
        Just winningBoard -> findWinner2 newBoards rest (Just (Tuple winningBoard number))
        Nothing -> findWinner2 newBoards rest lastWinner
  Nothing -> lastWinner

solve1 :: RawInput -> Array Int -> Maybe Int
solve1 boardInput numberInput = do
  boards <- makeBoards boardInput
  (Tuple (BingoBoard board) number) <- findWinner boards numberInput
  pure $ filter (\r -> r.marked == false) board.spaces <#> (\r -> r.number) # sum # mul number

solve2 :: RawInput -> Array Int -> Maybe Int
solve2 boardInput numberInput = do
  boards <- makeBoards boardInput
  (Tuple (BingoBoard board) number) <- findWinner2 boards numberInput Nothing
  pure $ filter (\r -> r.marked == false) board.spaces <#> (\r -> r.number) # sum # mul number
