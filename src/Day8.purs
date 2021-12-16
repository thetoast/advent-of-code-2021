module Day8 where

import Prelude

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

type ProgramState
  = { solvedDigits :: Map Int String
    , segmentDigits :: Map String Int
    , inputsByLen :: Map Int (Array String)
    , segsToLetters :: Map Int String
    }

containsSegNum :: ProgramState -> Int -> String -> Boolean
containsSegNum state num input = case lookup num state.segsToLetters of
  Just letter -> contains (Pattern letter) input
  Nothing -> false

containsDigit :: ProgramState -> Int -> String -> Boolean
containsDigit state digit input = case lookup digit state.solvedDigits of
  Just digitStr -> intersect (toCodePointArray digitStr) (toCodePointArray input) == (toCodePointArray digitStr)
  Nothing -> false

subSegments :: String -> String -> String
subSegments a b = difference (toCodePointArray a) (toCodePointArray b) # fromCodePointArray

intersectSegments :: String -> String -> String
intersectSegments a b = intersect (toCodePointArray a) (toCodePointArray b) # fromCodePointArray

markSolved :: ProgramState -> Int -> String -> ProgramState
markSolved state num solution =
  state
    { solvedDigits = insert num solution state.solvedDigits
    , segmentDigits = insert solution num state.segmentDigits
    , inputsByLen = update (\a -> Just (delete solution a)) (String.length solution) state.inputsByLen
    }

markSegment :: ProgramState -> Int -> String -> ProgramState
markSegment state num letter = state { segsToLetters = insert num letter state.segsToLetters }

cat2 :: forall a. Array a -> Array a -> Array a
cat2 a b = concat [ a, b ]

getInputsByLen :: SegmentNote -> Map Int (Array String)
getInputsByLen note = foldl (\m s -> insertWith cat2 (String.length s) [ s ] m) empty note.sequence

createState :: SegmentNote -> ProgramState
createState note = { solvedDigits: empty, segmentDigits: empty, inputsByLen: getInputsByLen note, segsToLetters: empty }

solveNumByLen :: Int -> Int -> ProgramState -> ProgramState
solveNumByLen number length state =
  unsafePartial
    $ markSolved state number
    $ fromJust
    $ lookup length state.inputsByLen
    >>= (\a -> a !! 0)

--find 1 by len
--find 4 by len
--find 7 by len
--find 8 by len
solveEasy :: ProgramState -> ProgramState
solveEasy = solveNumByLen 1 2 >>> solveNumByLen 4 4 >>> solveNumByLen 7 3 >>> solveNumByLen 8 7

findByLenSuperset :: Int -> Int -> Int -> ProgramState -> ProgramState
findByLenSuperset number length subset state =
  let
    lens = unsafePartial $ fromJust $ lookup length state.inputsByLen

    found = unsafePartial $ fromJust $ find (containsDigit state subset) lens
  in
    markSolved state number found

findByLenContainsSeg :: Int -> Int -> Int -> ProgramState -> ProgramState
findByLenContainsSeg number length segment state =
  let
    lens = unsafePartial $ fromJust $ lookup length state.inputsByLen

    found = unsafePartial $ fromJust $ find (containsSegNum state segment) lens
  in
    markSolved state number found

--find 3 by len 5 and containsSegLetters of 7
solve3 :: ProgramState -> ProgramState
solve3 = findByLenSuperset 3 5 7

--find 9 by len 6 and containsSegLetters of 3
solve9 :: ProgramState -> ProgramState
solve9 = findByLenSuperset 9 6 3

--solve seg 2 by sub num 3 from num 4
--solve seg 4 by by sub num 1 from num 4 then intersect with num 3
solveSegs24 :: ProgramState -> ProgramState
solveSegs24 state =
  let
    num1 = unsafePartial $ fromJust $ lookup 1 state.solvedDigits

    num3 = unsafePartial $ fromJust $ lookup 3 state.solvedDigits

    num4 = unsafePartial $ fromJust $ lookup 4 state.solvedDigits

    seg2 = subSegments num4 num3

    seg4 = subSegments num4 num1 # intersectSegments num3
  in
    state { segsToLetters = insert 2 seg2 state.segsToLetters # insert 4 seg4 }

--find 5 by len 5 and containsSegNum 2
solve5 :: ProgramState -> ProgramState
solve5 = findByLenContainsSeg 5 5 2

--find 6 as len 6 and containsSegNum 4
solve6 :: ProgramState -> ProgramState
solve6 = findByLenContainsSeg 6 6 4

--find 2 as remaining 5 seg
solve2 :: ProgramState -> ProgramState
solve2 = solveNumByLen 2 5

--find 0 as last len 6
solve0 :: ProgramState -> ProgramState
solve0 = solveNumByLen 0 6

solveNote :: SegmentNote -> Maybe Int
solveNote note =
  let
    state = solve0 $ solve6 $ solve2 $ solve5 $ solveSegs24 $ solve9 $ solve3 $ solveEasy $ createState note

    finalNum = traverse (\s -> lookup s state.segmentDigits) note.final <#> foldl (\a d -> a <> show d) ""
  in
    finalNum >>= fromString

solveInput :: String -> Maybe Int
solveInput input = do
  notes <- parseInput input
  outputs <- traverse solveNote notes
  pure $ sum outputs
