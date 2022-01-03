module Graph where

import Prelude

import Data.Map (Map, insertWith, lookup)
import Data.Map (empty) as M
import Data.Maybe (Maybe)
import Data.Set (Set, insert, singleton, toUnfoldable)
import Data.Set (empty) as S
import Data.Tuple (Tuple(..))

type Node :: forall k. k -> k
type Node a
  = a

type Graph a
  = { nodes :: Set (Node a)
    , edges :: Map (Node a) (Set (Node a))
    }

type Edge a
  = Tuple (Node a) (Node a)

type Path a
  = Array (Node a)

addEdge :: forall a. Ord a => Graph a -> Edge a -> Graph a
addEdge { nodes, edges } (Tuple a b) =
  { nodes: insert a nodes # insert b
  , edges: insertWith (<>) a (singleton b) edges # insertWith (<>) b (singleton a)
  }

nodeEdges :: forall a. Ord a => Node a -> Graph a -> Maybe (Array (Node a))
nodeEdges node { edges } = lookup node edges <#> toUnfoldable

emptyGraph :: forall a. Graph a
emptyGraph = { nodes: S.empty, edges: M.empty }
