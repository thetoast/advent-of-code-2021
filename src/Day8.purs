module Day8 where

import Prelude
import Control.Monad.State (State, execState, gets, modify_)
import Data.Array (any, concat, delete, difference, filter, find, intersect, sort, (!!))
import Data.Foldable (foldl, sum)
import Data.Int (fromString)
import Data.Map (Map, empty, insert, insertWith, lookup, update)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..), contains, fromCodePointArray, split, toCodePointArray)
import Data.String (length) as String
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)

type SegmentNote
  = { sequence :: Array String
    , final :: Array String
    }

type NoteState
  = { solvedDigits :: Map Int String
    , segmentDigits :: Map String Int
    , inputsByLen :: Map Int (Array String)
    , segsToLetters :: Map Int String
    }

type Day8State
  = State NoteState

parseInput :: String -> Maybe (Array SegmentNote)
parseInput input = split (Pattern "\n") input # filter (\l -> l /= "") # traverse makeNote
  where
  makeNote note = case split (Pattern " | ") note of
    [ sequence, final ] ->
      Just
        { sequence: split (Pattern " ") sequence <#> sortString
        , final: split (Pattern " ") final <#> sortString
        }
    _ -> Nothing

solve1 :: String -> Maybe Int
solve1 inputRaw = do
  input <- parseInput inputRaw
  pure $ foldl countEasy 0 input
  where
  countEasy sum note = sum + (foldl isEasy 0 note.final)

  isEasy acc digit = if String.length digit # (\len -> any (eq len) [ 2, 3, 4, 7 ]) then acc + 1 else acc

sortString :: String -> String
sortString = toCodePointArray >>> sort >>> fromCodePointArray

containsSegNum :: NoteState -> Int -> String -> Boolean
containsSegNum state num input = case lookup num state.segsToLetters of
  Just letter -> contains (Pattern letter) input
  Nothing -> false

containsDigit :: NoteState -> Int -> String -> Boolean
containsDigit state digit input = case lookup digit state.solvedDigits of
  Just digitStr -> intersect (toCodePointArray digitStr) (toCodePointArray input) == (toCodePointArray digitStr)
  Nothing -> false

subSegments :: String -> String -> String
subSegments a b = difference (toCodePointArray a) (toCodePointArray b) # fromCodePointArray

intersectSegments :: String -> String -> String
intersectSegments a b = intersect (toCodePointArray a) (toCodePointArray b) # fromCodePointArray

markSolved :: Int -> String -> Day8State Unit
markSolved num solution =
  modify_ \state ->
    state
      { solvedDigits = insert num solution state.solvedDigits
      , segmentDigits = insert solution num state.segmentDigits
      , inputsByLen = update (\a -> Just (delete solution a)) (String.length solution) state.inputsByLen
      }

cat2 :: forall a. Array a -> Array a -> Array a
cat2 a b = concat [ a, b ]

getInputsByLen :: SegmentNote -> Map Int (Array String)
getInputsByLen note = foldl (\m s -> insertWith cat2 (String.length s) [ s ] m) empty note.sequence

createState :: SegmentNote -> NoteState
createState note = { solvedDigits: empty, segmentDigits: empty, inputsByLen: getInputsByLen note, segsToLetters: empty }

solveNumByLen :: Int -> Int -> Day8State Unit
solveNumByLen number length = markSolved number =<< gets \state -> unsafePartial $ fromJust $ lookup length state.inputsByLen >>= (\a -> a !! 0)

--find 1 by len
--find 4 by len
--find 7 by len
--find 8 by len
solveEasy :: Day8State Unit
solveEasy = solveNumByLen 1 2 <> solveNumByLen 4 4 <> solveNumByLen 7 3 <> solveNumByLen 8 7

solveByLenSuperset :: Int -> Int -> Int -> Day8State Unit
solveByLenSuperset number length subset = do
  lens <- gets \state -> unsafePartial $ fromJust $ lookup length state.inputsByLen
  found <- gets \state -> unsafePartial $ fromJust $ find (containsDigit state subset) lens
  markSolved number found

solveByLenContainsSeg :: Int -> Int -> Int -> Day8State Unit
solveByLenContainsSeg number length segment = do
  lens <- gets \state -> unsafePartial $ fromJust $ lookup length state.inputsByLen
  found <- gets \state -> unsafePartial $ fromJust $ find (containsSegNum state segment) lens
  markSolved number found

--find 3 by len 5 and containsSegLetters of 7
solve3 :: Day8State Unit
solve3 = solveByLenSuperset 3 5 7

--find 9 by len 6 and containsSegLetters of 3
solve9 :: Day8State Unit
solve9 = solveByLenSuperset 9 6 3

--solve seg 2 by sub num 3 from num 4
--solve seg 4 by by sub num 1 from num 4 then intersect with num 3
solveSegs24 :: Day8State Unit
solveSegs24 = do
  num1 <- gets \state -> unsafePartial $ fromJust $ lookup 1 state.solvedDigits
  num3 <- gets \state -> unsafePartial $ fromJust $ lookup 3 state.solvedDigits
  num4 <- gets \state -> unsafePartial $ fromJust $ lookup 4 state.solvedDigits
  seg2 <- pure $ subSegments num4 num3
  seg4 <- pure $ subSegments num4 num1 # intersectSegments num3
  modify_ \state -> state { segsToLetters = insert 2 seg2 state.segsToLetters # insert 4 seg4 }

--find 5 by len 5 and containsSegNum 2
solve5 :: Day8State Unit
solve5 = solveByLenContainsSeg 5 5 2

--find 6 as len 6 and containsSegNum 4
solve6 :: Day8State Unit
solve6 = solveByLenContainsSeg 6 6 4

--find 2 as remaining 5 seg
solve2 :: Day8State Unit
solve2 = solveNumByLen 2 5

--find 0 as last len 6
solve0 :: Day8State Unit
solve0 = solveNumByLen 0 6

solveNote :: SegmentNote -> Maybe Int
solveNote note =
  let
    comps = solveEasy <> solve3 <> solve9 <> solveSegs24 <> solve5 <> solve2 <> solve6 <> solve0

    finalState = execState comps $ createState note

    finalNum = traverse (\s -> lookup s finalState.segmentDigits) note.final <#> foldl (\a d -> a <> show d) ""
  in
    finalNum >>= fromString

solveInput :: String -> Maybe Int
solveInput input = do
  notes <- parseInput input
  outputs <- traverse solveNote notes
  pure $ sum outputs
