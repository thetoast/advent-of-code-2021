module Day12 where

import Prelude
import Data.Array (concat, elem, filter, length, snoc)
import Data.Foldable (foldl)
import Data.Map (Map, insertWith, lookup)
import Data.Map (empty) as M
import Data.Maybe (Maybe(..))
import Data.Set (Set, insert, member)
import Data.Set (empty) as S
import Data.String (split, Pattern(..))
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

type Node
  = String

type Graph
  = { nodes :: Set Node
    , edges :: Map Node (Array Node)
    }

type Edge
  = Tuple Node Node

type Path
  = Array Node

addEdge :: Graph -> Edge -> Graph
addEdge { nodes, edges } (Tuple a b) =
  { nodes: insert a nodes # insert b
  , edges: insertWith (<>) a [ b ] edges # insertWith (<>) b [ a ]
  }

makeTuple :: Array Node -> Maybe (Tuple Node Node)
makeTuple [ a, b ] = Just (Tuple a b)

makeTuple _ = Nothing

parseInput :: String -> Maybe Graph
parseInput input =
  split (Pattern "\n") input
    # traverse (split (Pattern "-") >>> makeTuple)
    <#> foldl addEdge { nodes: S.empty, edges: M.empty }

bigCave :: Regex
bigCave = unsafeRegex "[A-Z]+" noFlags

isBig :: Node -> Boolean
isBig = test bigCave

startCave :: String
startCave = "start"

isStart :: Node -> Boolean
isStart = eq startCave

endCave :: String
endCave = "end"

isEnd :: Node -> Boolean
isEnd = eq endCave

nodeEdges :: Node -> Graph -> Maybe (Array Node)
nodeEdges node { edges } = lookup node edges

findPaths1 :: Graph -> Path -> Node -> Maybe (Array Path)
findPaths1 graph history node = do
  nexts <- nodeEdges node graph <#> filter (\n -> isBig n || not (elem n history))
  newHistory <- pure $ snoc history node
  if isEnd node then
    Just [ newHistory ]
  else
    concat <$> traverse (findPaths1 graph newHistory) nexts

hasDoubleSmall :: Path -> Boolean
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

canVisit :: Path -> Node -> Boolean
canVisit history node
  | isStart node = false
  | not (isBig node) && elem node history && hasDoubleSmall history = false
  | otherwise = true

solve1 :: String -> Maybe Int
solve1 input = do
  graph <- parseInput input
  paths <- findPaths1 graph [] "start"
  pure $ length paths

findPaths2 :: Graph -> Path -> Node -> Maybe (Array Path)
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
