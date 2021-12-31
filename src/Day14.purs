module Day14 where

import Prelude
import Data.Array (concat, drop, snoc, take, (!!))
import Data.Foldable (foldM, foldl, maximum, minimum)
import Data.List (List)
import Data.Map (Map, empty, insert, insertWith, lookup, values)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, Pattern(..), codePointAt, split, toCodePointArray)
import Data.Tuple (Tuple(..), fst)

type Pair
  = Tuple CodePoint CodePoint

type Insertions
  = Map Pair CodePoint

type Polymer
  = Array CodePoint

makePair :: Array CodePoint -> Maybe Pair
makePair pair = do
  first <- pair !! 0
  second <- pair !! 1
  pure $ Tuple first second

insertRule :: Insertions -> Array String -> Maybe Insertions
insertRule inserts rule = do
  pair <- makePair =<< toCodePointArray <$> rule !! 0
  replacement <- codePointAt 0 =<< rule !! 1
  pure $ insert pair replacement inserts

parseRules :: String -> Maybe Insertions
parseRules = split (Pattern "\n") >>> map (split (Pattern " -> ")) >>> foldM insertRule empty

checkNextPair :: Insertions -> Polymer -> Pair -> Polymer
checkNextPair inserts polymer pair = case lookup pair inserts of
  Just insertion -> concat [ polymer, [ (fst pair), insertion ] ]
  Nothing -> polymer

processPolymer :: Insertions -> Polymer -> Polymer -> Polymer
processPolymer inserts newPolymer oldPolymer = case take 2 oldPolymer of
  [ a, b ] ->
    let
      pair = Tuple a b

      rest = drop 1 oldPolymer

      nextPolymer = checkNextPair inserts newPolymer pair
    in
      processPolymer inserts nextPolymer rest
  [ a ] -> snoc newPolymer a
  _ -> newPolymer

step :: Int -> Insertions -> Polymer -> Polymer
step num inserts polymer
  | num == 0 = polymer
  | otherwise = step (num - 1) inserts $ processPolymer inserts [] polymer

makeCounts :: Polymer -> List Int
makeCounts polymer =
  let
    map = foldl (\m c -> insertWith (+) c 1 m) empty polymer
  in
    values map

solve1 :: String -> String -> Maybe Int
solve1 input rules = do
  inserts <- parseRules rules
  polymer <- pure $ toCodePointArray input
  counts <- pure $ makeCounts $ step 10 inserts polymer
  max <- maximum counts
  min <- minimum counts
  pure (max - min)

solve2 :: Int -> String -> String -> Maybe Int
solve2 times input rules = do
  inserts <- parseRules rules
  polymer <- pure $ toCodePointArray input
  counts <- pure $ makeCounts $ step times inserts polymer
  max <- maximum counts
  min <- minimum counts
  pure (max - min)
