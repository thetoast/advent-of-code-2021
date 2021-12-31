module Day14 where

import Prelude

import Data.Array (head, last, uncons, (!!))
import Data.Foldable (foldM, foldl, maximum, minimum)
import Data.List (List)
import Data.Map (Map, empty, insert, insertWith, keys, lookup, toUnfoldable, values)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, Pattern(..), codePointAt, split, toCodePointArray)
import Data.Tuple (Tuple(..))

type Pair
  = Tuple CodePoint CodePoint

type Insertions
  = Map Pair CodePoint

type Polymer
  = Array CodePoint

type PairCounts
  = Map Pair Number

type CodeCounts
  = Map CodePoint Number

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

getPointCounts :: CodePoint -> CodePoint -> PairCounts -> List Number
getPointCounts first last pairs =
  let
    unfolded :: Array (Tuple Pair Number)
    unfolded = toUnfoldable pairs

    counts :: CodeCounts
    counts = foldl (\m (Tuple (Tuple f s) n) -> insertWith (+) f n m # insertWith (+) s n) empty unfolded
  in
    insertWith (+) first 1.0 counts # insertWith (+) last 1.0 # values <#> \i -> div i 2.0

processPair :: Insertions -> PairCounts -> PairCounts -> Pair -> Maybe PairCounts
processPair inserts oldCounts newCounts pair@(Tuple f s) = do
  insertion <- lookup pair inserts
  pairCount <- lookup pair oldCounts
  pair1 <- pure $ Tuple f insertion
  pair2 <- pure $ Tuple insertion s
  pure $ insertWith (+) pair1 pairCount $ insertWith (+) pair2 pairCount newCounts

makePairCounts :: Polymer -> PairCounts
makePairCounts polymer = case uncons polymer of
  Nothing -> empty
  Just { head, tail } ->
    foldl
      ( \a c ->
          { counts: insertWith (+) (Tuple a.last c) 1.0 a.counts
          , last: c
          }
      )
      { counts: empty, last: head }
      tail
      # _.counts

processPolymerCounts :: Int -> Insertions -> PairCounts -> Maybe PairCounts
processPolymerCounts times inserts counts
  | times == 0 = Just counts
  | otherwise = case foldM (processPair inserts counts) empty $ keys counts of
    Just newCounts -> processPolymerCounts (times - 1) inserts newCounts
    Nothing -> Just counts

solve :: Int -> String -> String -> Maybe Number
solve times input rules = do
  inserts <- parseRules rules
  polymer <- pure $ toCodePointArray input
  first <- head polymer
  last <- last polymer
  initialPairCounts <- pure $ makePairCounts polymer
  finalPointCounts <- getPointCounts first last <$> processPolymerCounts times inserts initialPairCounts
  max <- maximum finalPointCounts
  min <- minimum finalPointCounts
  pure (max - min)
