module Day12 where

import Prelude

import Data.Array (concat, elem, filter, length, snoc)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Set (insert, member)
import Data.Set (empty) as S
import Data.String (split, Pattern(..))
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Graph (Graph, Node, Path, addEdge, emptyGraph, nodeEdges)

type Cave
  = Node String

type CaveGraph
  = Graph Cave

makeTuple :: Array Cave -> Maybe (Tuple Cave Cave)
makeTuple [ a, b ] = Just (Tuple a b)

makeTuple _ = Nothing

parseInput :: String -> Maybe CaveGraph
parseInput input =
  split (Pattern "\n") input
    # traverse (split (Pattern "-") >>> makeTuple)
    <#> foldl addEdge emptyGraph

bigCave :: Regex
bigCave = unsafeRegex "[A-Z]+" noFlags

isBig :: Cave -> Boolean
isBig = test bigCave

startCave :: String
startCave = "start"

isStart :: Cave -> Boolean
isStart = eq startCave

endCave :: String
endCave = "end"

isEnd :: Cave -> Boolean
isEnd = eq endCave

findPaths1 :: CaveGraph -> Path Cave -> Cave -> Maybe (Array (Path Cave))
findPaths1 graph history node = do
  nexts <- nodeEdges node graph <#> filter (\n -> isBig n || not (elem n history))
  newHistory <- pure $ snoc history node
  if isEnd node then
    Just [ newHistory ]
  else
    concat <$> traverse (findPaths1 graph newHistory) nexts

hasDoubleSmall :: Path Cave -> Boolean
hasDoubleSmall =
  foldl
    ( \a@{ set } n ->
        if isBig n then
          a
        else if member n set then
          a { double = true }
        else
          a { set = insert n set }
    )
    { set: S.empty, double: false }
    >>> _.double

canVisit :: Path Cave -> Cave -> Boolean
canVisit history node
  | isStart node = false
  | not (isBig node) && elem node history && hasDoubleSmall history = false
  | otherwise = true

solve1 :: String -> Maybe Int
solve1 input = do
  graph <- parseInput input
  paths <- findPaths1 graph [] "start"
  pure $ length paths

findPaths2 :: CaveGraph -> Path Cave -> Cave -> Maybe (Array (Path Cave))
findPaths2 graph history node = do
  newHistory <- pure $ snoc history node
  nexts <- nodeEdges node graph <#> filter (canVisit newHistory)
  if isEnd node then
    Just [ newHistory ]
  else
    concat <$> traverse (findPaths2 graph newHistory) nexts

solve2 :: String -> Maybe Int
solve2 input = do
  graph <- parseInput input
  paths <- findPaths2 graph [] "start"
  pure $ length paths
