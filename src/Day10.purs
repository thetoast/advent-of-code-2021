module Day10 where

import Prelude
import Data.Array (cons, filter, length, mapMaybe, uncons, zip)
import Data.Foldable (foldl, sum)
import Data.Map (Map, lookup)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Set (Set, member)
import Data.Set (fromFoldable, toUnfoldable) as Set
import Data.String (CodePoint, fromCodePointArray, split, toCodePointArray, Pattern(..), codePointFromChar)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Stats (medianN)

type Expected
  = CodePoint

type Actual
  = CodePoint

data ParserResult
  = Complete
  | Incomplete
  | Corrupted Expected Actual

isComplete :: ParserResult -> Boolean
isComplete Complete = true

isComplete _ = false

isIncomplete :: ParserResult -> Boolean
isIncomplete Incomplete = true

isIncomplete _ = false

isCorrupted :: ParserResult -> Boolean
isCorrupted (Corrupted _ _) = true

isCorrupted _ = false

instance showParserResult :: Show ParserResult where
  show Complete = "complete"
  show Incomplete = "incomplete"
  show (Corrupted e a) = "corrupted: expected=" <> fromCodePointArray [ e ] <> " actual=" <> fromCodePointArray [ a ]

type ParserState
  = { groups :: Array CodePoint
    , result :: Maybe ParserResult
    }

openings :: Set CodePoint
openings = Set.fromFoldable $ toCodePointArray $ "([{<"

closings :: Set CodePoint
closings = Set.fromFoldable $ toCodePointArray $ ")]}>"

scores :: Map CodePoint Int
scores =
  [ Tuple (codePointFromChar ')') 3
  , Tuple (codePointFromChar ']') 57
  , Tuple (codePointFromChar '}') 1197
  , Tuple (codePointFromChar '>') 25137
  ]
    # Map.fromFoldable

scores2 :: Map CodePoint Number
scores2 =
  [ Tuple (codePointFromChar ')') 1.0
  , Tuple (codePointFromChar ']') 2.0
  , Tuple (codePointFromChar '}') 3.0
  , Tuple (codePointFromChar '>') 4.0
  ]
    # Map.fromFoldable

closingChar :: Map CodePoint CodePoint
closingChar = zip (Set.toUnfoldable openings) (Set.toUnfoldable closings) # Map.fromFoldable

getClosing :: CodePoint -> CodePoint
getClosing o = unsafePartial $ fromJust $ lookup o closingChar

parseString :: String -> ParserState
parseString input =
  let
    finalState@{ result } =
      foldl
        ( \s c -> case s.result of
            Just _ -> s
            Nothing ->
              if member c openings then
                s { groups = cons (getClosing c) s.groups }
              else case uncons s.groups of
                Nothing -> s { result = Just Incomplete }
                Just { head, tail } ->
                  if head == c then
                    s { groups = tail }
                  else
                    s { result = Just (Corrupted head c) }
        )
        { groups: [], result: Nothing }
        (toCodePointArray input)
  in
    case result of
      Nothing -> finalState { result = Just if (length finalState.groups) > 0 then Incomplete else Complete }
      _ -> finalState

getScore :: ParserResult -> Maybe Int
getScore (Corrupted _ a) = lookup a scores

getScore _ = Nothing

solve1 :: String -> Maybe Int
solve1 input = do
  results <- sequence $ _.result <$> parseString <$> split (Pattern "\n") input
  pure $ mapMaybe getScore results # sum

scoreClosers :: Array CodePoint -> Maybe Number
scoreClosers closers = traverse (\c -> lookup c scores2) closers <#> foldl (\s v -> (s * 5.0) + v) 0.0

isStateIncomplete :: ParserState -> Boolean
isStateIncomplete { result: (Just Incomplete) } = true
isStateIncomplete _ = false

solve2 :: String -> Maybe Number
solve2 input = 
  let
    closers = _.groups <$> (filter isStateIncomplete $ parseString <$> split (Pattern "\n") input)
  in
     traverse scoreClosers closers >>= medianN
