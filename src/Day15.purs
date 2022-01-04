module Day15 where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Foldable (foldM, foldl)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (empty, insert) as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Geometry (Grid, NeighborType(..), Point(..), Dimensions(..), gridDimensions, gridFromIntStrings, gridPoints, gridValueAt, validNeighbors)
import Graph (Graph, Node, addEdge, emptyGraph, nodeEdges)
import PriorityQueue (PriorityQueue)
import PriorityQueue as PQ

type RiskPoint
  = Node { point :: Point, level :: Int }

type RawRisks
  = Grid Int

type Cavern
  = { start :: RiskPoint
    , end :: RiskPoint
    , graph :: Graph RiskPoint
    }

type RiskMap
  = Map RiskPoint Int

type Frontier
  = PriorityQueue Int RiskPoint

type TraversalState
  = { visited :: Set RiskPoint
    , frontier :: Frontier
    , cavern :: Cavern
    }

makeNode :: RawRisks -> Point -> Maybe RiskPoint
makeNode grid point = do
  level <- gridValueAt point grid
  Just { point, level }

addEdges :: Graph RiskPoint -> RiskPoint -> Array RiskPoint -> Graph RiskPoint
addEdges graph node neighbors = foldl (\c n -> addEdge c (Tuple node n)) graph neighbors

addPoint :: RawRisks -> Graph RiskPoint -> Point -> Maybe (Graph RiskPoint)
addPoint grid graph point = do
  node <- makeNode grid point
  neighbors <- traverse (makeNode grid) (validNeighbors point Adjacent grid)
  pure $ addEdges graph node neighbors

parseInput :: String -> Maybe Cavern
parseInput input = do
  grid <- gridFromIntStrings input
  allPoints <- pure $ gridPoints grid
  graph <- foldM (addPoint grid) emptyGraph allPoints
  startPoint <- pure $ Point { x: 0, y: 0 }
  endPoint <- (\(Dimensions { width, height }) -> Point { x: width - 1, y: height - 1 }) <$> gridDimensions grid
  start <- gridValueAt startPoint grid <#> \level -> { level, point: startPoint }
  end <- gridValueAt endPoint grid <#> \level -> { level, point: endPoint }
  pure { start, end, graph }

updateRisk :: Int -> Frontier -> RiskPoint -> Frontier
updateRisk currentLevel frontier neighbor =
  let
    neighborLevel = PQ.priority neighbor frontier

    newLevel = case neighborLevel of
      Just current -> min current (currentLevel + neighbor.level)
      Nothing -> currentLevel + neighbor.level
  in
    PQ.push newLevel neighbor frontier

visit :: TraversalState -> RiskPoint -> Maybe TraversalState
visit state point = do
  currentRisk <- PQ.priority point state.frontier
  newFrontier <-
    pure
      $ case nodeEdges point state.cavern.graph of
          Nothing -> state.frontier
          Just neighbors -> foldl (updateRisk currentRisk) state.frontier neighbors
  pure
    $ state
        { visited = S.insert point state.visited
        , frontier = PQ.delete point newFrontier
        }

lowestUnvisited :: TraversalState -> Maybe RiskPoint
lowestUnvisited { frontier } = PQ.peekMin frontier

makeVisits :: TraversalState -> RiskPoint -> Maybe TraversalState
makeVisits state@{ cavern: { end } } point = tailRecM go { state, point }
  where
  go { state: state', point: point' }
    | point' == end = Just (Done state')
    | otherwise = do
      newState <- visit state' point'
      next <- lowestUnvisited newState
      Just (Loop { state: newState, point: next })

makeState :: Cavern -> TraversalState
makeState cavern@{ start } =
  { visited: S.empty
  , frontier: PQ.push 0 start PQ.empty
  , cavern
  }

findLowestRisk :: Cavern -> Maybe Int
findLowestRisk cavern@{ start, end } = do
  initState <- pure $ makeState cavern
  finalState <- makeVisits initState start
  PQ.priority end finalState.frontier

solve1 :: String -> Maybe Int
solve1 input = parseInput input >>= findLowestRisk
