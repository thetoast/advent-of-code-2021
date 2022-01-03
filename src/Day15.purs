module Day15 where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (filter, uncons)
import Data.Foldable (foldM, foldl)
import Data.Map (Map, lookup)
import Data.Map (empty, insert) as M
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set, delete, toUnfoldable)
import Data.Set (empty, insert) as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Geometry (Grid, NeighborType(..), Point(..), Dimensions(..), gridDimensions, gridFromIntStrings, gridPoints, gridValueAt, validNeighbors)
import Graph (Graph, Node, addEdge, emptyGraph, nodeEdges)

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

type TraversalState
  = { visited :: Set RiskPoint
    , unvisited :: Set RiskPoint
    , lowestRisk :: RiskMap
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

updateRisk :: Int -> RiskMap -> RiskPoint -> RiskMap
updateRisk currentLevel risks neighbor =
  let
    neighborLevel = lookup neighbor risks

    newLevel = case neighborLevel of
      Just current -> min current (currentLevel + neighbor.level)
      Nothing -> currentLevel + neighbor.level
  in
    M.insert neighbor newLevel risks

visit :: TraversalState -> RiskPoint -> Maybe TraversalState
visit state point = do
  currentRisk <- lookup point state.lowestRisk
  newRisks <-
    pure
      $ case nodeEdges point state.cavern.graph of
          Nothing -> state.lowestRisk
          Just neighbors -> foldl (updateRisk currentRisk) state.lowestRisk neighbors
  pure
    $ state
        { visited = S.insert point state.visited
        , unvisited = delete point state.unvisited
        , lowestRisk = newRisks
        }

lowestUnvisited :: TraversalState -> Maybe RiskPoint
lowestUnvisited { lowestRisk, unvisited } = do
  canVisit <- pure $ filter (\n -> isJust $ lookup n lowestRisk) $ toUnfoldable unvisited
  { head } <- uncons canVisit
  risk <- lookup head lowestRisk
  initState <- pure { best: head, risk }
  check <-
    pure
      ( \a u -> do
          unvisitedRisk <- lookup u lowestRisk
          if unvisitedRisk < a.risk then Just a { best = u, risk = unvisitedRisk } else Just a
      )
  _.best <$> foldM check initState canVisit

--makeVisits :: TraversalState -> RiskPoint -> Maybe TraversalState
--makeVisits state@{ cavern: { end } } point =
--  if point == end then
--    Just state
--  else do
--    newState <- visit state point
--    next <- lowestUnvisited newState
--    makeVisits newState next

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
makeState cavern@{ graph, start } =
  { visited: S.empty
  , unvisited: graph.nodes
  , lowestRisk: M.insert start 0 M.empty
  , cavern
  }

findLowestRisk :: Cavern -> Maybe Int
findLowestRisk cavern@{ start, end } = do
  initState <- pure $ makeState cavern
  finalState <- makeVisits initState start
  lookup end finalState.lowestRisk

solve1 :: String -> Maybe Int
solve1 input = parseInput input >>= findLowestRisk
