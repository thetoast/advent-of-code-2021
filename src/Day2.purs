module Day2 where

import Prelude
import Data.Array ((!!))
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

testCommands =
  [ "forward 5"
  , "down 5"
  , "forward 8"
  , "up 3"
  , "down 8"
  , "forward 2"
  ] ::
    Array String

realCommands = [ "forward 6", "forward 8", "down 6", "down 6", "forward 9", "down 7", "down 3", "forward 4", "forward 7", "up 3", "forward 9", "up 4", "forward 4", "down 3", "up 2", "forward 2", "down 4", "forward 9", "up 4", "forward 8", "up 9", "forward 3", "down 7", "down 4", "down 7", "up 5", "down 5", "down 5", "forward 5", "up 9", "down 5", "down 3", "up 3", "forward 2", "forward 2", "down 2", "forward 1", "down 4", "down 1", "down 6", "down 9", "forward 4", "forward 7", "down 4", "up 7", "forward 8", "down 3", "down 6", "down 5", "down 2", "down 1", "up 9", "forward 2", "forward 3", "up 3", "forward 5", "down 4", "up 3", "up 9", "forward 6", "up 5", "up 8", "up 8", "forward 6", "up 1", "forward 4", "down 4", "forward 9", "down 9", "down 7", "forward 5", "down 7", "forward 7", "up 2", "down 9", "down 2", "forward 8", "forward 9", "down 4", "down 4", "down 7", "down 4", "down 8", "forward 4", "down 9", "down 6", "forward 8", "down 8", "down 8", "forward 7", "up 9", "forward 7", "up 3", "down 5", "forward 9", "down 7", "down 5", "down 8", "forward 5", "down 1", "down 5", "down 5", "forward 8", "down 8", "up 8", "down 6", "down 3", "forward 1", "down 2", "down 3", "up 4", "forward 8", "down 6", "up 8", "forward 2", "forward 8", "forward 9", "forward 5", "forward 5", "forward 2", "forward 1", "down 8", "forward 8", "forward 9", "up 8", "up 8", "down 7", "down 5", "forward 1", "forward 1", "up 9", "up 3", "forward 5", "down 8", "down 4", "up 2", "down 7", "down 4", "down 3", "forward 3", "forward 7", "forward 3", "forward 1", "forward 8", "down 9", "up 4", "down 8", "down 3", "down 9", "up 8", "up 6", "up 9", "up 2", "down 9", "down 9", "down 6", "up 3", "down 4", "down 3", "down 9", "forward 2", "forward 2", "forward 7", "forward 9", "up 5", "up 8", "forward 8", "down 8", "up 7", "forward 4", "up 7", "up 7", "down 9", "down 7", "down 1", "forward 4", "forward 6", "up 4", "forward 7", "up 4", "down 6", "forward 2", "up 4", "forward 2", "up 2", "down 4", "down 2", "forward 4", "forward 8", "down 9", "up 7", "forward 3", "down 5", "down 2", "down 7", "up 9", "down 2", "up 5", "down 9", "down 4", "forward 8", "forward 9", "down 7", "up 3", "down 4", "up 3", "down 3", "down 3", "up 5", "forward 7", "down 8", "down 1", "up 7", "forward 8", "forward 9", "forward 3", "forward 1", "up 4", "forward 4", "down 6", "forward 3", "down 9", "forward 2", "up 8", "down 4", "forward 7", "forward 1", "up 9", "forward 6", "down 1", "down 8", "forward 7", "forward 8", "forward 2", "up 6", "forward 3", "forward 2", "forward 7", "down 6", "up 8", "down 2", "down 8", "forward 6", "forward 1", "up 9", "down 8", "down 4", "forward 3", "down 2", "forward 1", "forward 8", "down 9", "down 9", "forward 3", "down 5", "down 5", "down 1", "down 3", "up 3", "down 6", "forward 9", "forward 7", "up 5", "forward 8", "up 1", "up 4", "down 8", "up 3", "forward 1", "up 9", "forward 5", "down 8", "down 1", "up 3", "down 1", "down 7", "forward 2", "down 9", "forward 3", "down 2", "up 5", "down 5", "forward 9", "down 2", "down 3", "forward 4", "forward 2", "down 5", "forward 9", "down 5", "forward 9", "down 9", "down 8", "forward 8", "forward 9", "down 2", "forward 4", "forward 9", "up 1", "down 2", "down 5", "forward 8", "forward 5", "up 5", "forward 2", "up 8", "forward 3", "down 2", "forward 1", "forward 7", "forward 7", "forward 1", "up 9", "down 5", "down 3", "down 7", "forward 2", "up 5", "forward 1", "forward 7", "up 7", "down 3", "up 5", "down 1", "forward 9", "down 7", "forward 4", "forward 5", "down 1", "down 5", "up 7", "forward 1", "down 4", "down 8", "down 6", "forward 8", "forward 4", "forward 2", "up 8", "down 5", "down 2", "forward 2", "down 1", "down 8", "down 5", "forward 5", "forward 8", "down 9", "forward 1", "forward 3", "down 3", "down 1", "down 5", "forward 6", "forward 6", "down 3", "forward 5", "forward 8", "down 9", "down 9", "forward 4", "forward 4", "forward 1", "forward 7", "down 6", "up 2", "down 2", "up 6", "forward 7", "forward 8", "up 3", "down 7", "up 9", "down 5", "down 7", "down 7", "forward 3", "down 3", "up 9", "up 5", "forward 4", "down 7", "down 7", "forward 1", "down 1", "down 2", "forward 8", "down 9", "down 2", "forward 3", "forward 5", "down 4", "up 7", "forward 1", "forward 4", "forward 7", "forward 1", "up 1", "down 6", "forward 2", "down 1", "forward 2", "forward 7", "forward 7", "up 3", "down 8", "forward 6", "forward 1", "down 6", "down 8", "down 1", "up 3", "up 6", "down 6", "down 1", "up 8", "forward 6", "up 6", "forward 5", "up 4", "up 7", "up 4", "down 5", "forward 2", "forward 7", "down 1", "forward 6", "down 5", "up 3", "up 4", "forward 1", "up 4", "forward 7", "up 5", "up 1", "up 2", "forward 4", "forward 2", "forward 7", "down 6", "forward 1", "up 5", "up 2", "down 8", "forward 5", "up 2", "down 9", "up 4", "down 2", "down 1", "forward 8", "down 2", "up 8", "down 8", "up 7", "up 4", "up 2", "forward 8", "up 1", "up 6", "forward 8", "forward 8", "forward 2", "down 5", "up 2", "down 1", "forward 2", "up 5", "forward 4", "forward 9", "down 1", "down 6", "forward 4", "down 3", "down 7", "down 5", "down 6", "up 6", "forward 4", "down 2", "down 4", "down 3", "forward 3", "forward 2", "up 7", "forward 7", "forward 6", "forward 3", "forward 1", "forward 8", "forward 3", "forward 2", "forward 3", "forward 9", "down 3", "forward 9", "up 9", "up 1", "down 8", "down 2", "up 9", "down 8", "forward 5", "down 8", "up 8", "down 3", "up 7", "down 3", "down 7", "up 9", "up 8", "down 5", "down 1", "forward 1", "down 9", "forward 5", "down 1", "forward 5", "forward 6", "up 2", "forward 5", "down 2", "up 3", "forward 8", "forward 9", "down 1", "up 9", "forward 6", "down 9", "down 2", "forward 2", "forward 4", "forward 9", "forward 3", "down 6", "down 9", "forward 8", "forward 3", "down 9", "up 6", "forward 5", "forward 8", "down 7", "forward 9", "forward 5", "forward 4", "forward 3", "down 9", "forward 7", "forward 7", "forward 8", "down 7", "down 3", "up 5", "forward 2", "up 6", "down 6", "down 5", "up 2", "down 1", "forward 7", "forward 3", "down 2", "up 4", "forward 8", "down 8", "down 2", "forward 8", "up 8", "forward 1", "forward 1", "down 1", "forward 4", "forward 7", "forward 2", "forward 6", "down 9", "down 2", "up 7", "up 5", "forward 4", "down 3", "down 4", "up 7", "forward 7", "up 6", "forward 6", "up 4", "forward 5", "forward 7", "forward 3", "down 2", "forward 2", "forward 2", "up 1", "down 8", "up 7", "down 4", "down 9", "forward 2", "forward 9", "down 7", "down 4", "down 8", "forward 9", "up 1", "forward 6", "forward 7", "down 3", "forward 9", "forward 3", "down 3", "up 2", "down 8", "down 7", "forward 1", "down 9", "up 3", "forward 6", "forward 1", "forward 7", "down 3", "forward 2", "forward 1", "down 8", "down 7", "forward 1", "forward 5", "down 7", "up 7", "forward 4", "down 3", "forward 7", "forward 2", "up 2", "forward 1", "up 2", "up 6", "forward 3", "forward 2", "down 3", "down 7", "down 2", "down 9", "down 6", "forward 8", "down 4", "forward 6", "up 7", "down 8", "up 5", "down 8", "up 6", "down 2", "down 1", "forward 9", "forward 8", "down 6", "down 3", "up 8", "down 1", "forward 2", "down 6", "forward 4", "forward 9", "forward 6", "down 2", "down 9", "up 3", "forward 8", "forward 1", "forward 6", "forward 8", "forward 4", "forward 8", "up 7", "down 7", "forward 7", "down 6", "forward 1", "down 9", "forward 5", "down 3", "down 6", "down 6", "down 9", "forward 8", "forward 5", "down 4", "forward 1", "down 8", "down 5", "down 5", "down 9", "forward 1", "forward 1", "forward 9", "forward 5", "down 4", "forward 3", "down 2", "forward 3", "up 7", "down 2", "down 5", "down 4", "up 2", "forward 7", "down 3", "down 4", "down 8", "down 2", "forward 3", "up 8", "down 9", "down 1", "down 2", "forward 3", "down 1", "down 7", "down 9", "forward 9", "forward 7", "down 2", "down 5", "up 8", "forward 1", "forward 2", "forward 6", "up 3", "forward 3", "down 6", "forward 1", "up 8", "down 3", "down 1", "forward 6", "up 7", "forward 3", "down 5", "down 5", "up 8", "down 4", "forward 1", "up 5", "down 4", "forward 5", "up 6", "forward 2", "up 4", "forward 9", "forward 1", "forward 9", "forward 5", "forward 9", "up 2", "forward 8", "forward 6", "down 5", "forward 9", "forward 7", "down 1", "forward 7", "forward 7", "forward 3", "up 9", "down 2", "up 7", "down 1", "up 7", "forward 6", "forward 2", "forward 5", "forward 6", "down 4", "down 8", "forward 6", "down 1", "forward 7", "up 8", "forward 7", "up 5", "down 2", "forward 9", "forward 7", "forward 8", "up 2", "down 9", "forward 5", "forward 8", "forward 5", "forward 7", "down 8", "down 8", "up 4", "down 8", "up 2", "up 6", "down 2", "forward 2", "forward 9", "forward 8", "forward 3", "up 3", "up 1", "down 3", "forward 7", "down 2", "up 2", "forward 5", "down 3", "forward 2", "forward 4", "up 2", "down 1", "down 5", "up 6", "down 7", "up 7", "up 6", "forward 5", "down 7", "forward 9", "forward 2", "up 8", "down 1", "down 6", "forward 9", "down 8", "up 7", "down 6", "forward 9", "forward 2", "down 1", "up 4", "up 7", "down 7", "up 6", "forward 4", "up 1", "forward 5", "forward 9", "down 3", "up 2", "down 3", "up 7", "down 8", "forward 1", "down 9", "down 1", "down 9", "forward 8", "down 4", "down 7", "down 7", "forward 4", "up 5", "forward 1", "forward 7", "down 9", "forward 6", "up 8", "down 5", "down 8", "up 9", "forward 6", "down 1", "forward 8", "up 8", "up 1", "up 7", "down 9", "forward 8", "down 8", "forward 4", "down 1", "down 5", "forward 9", "down 7", "forward 3", "forward 1", "forward 2", "down 9", "up 2", "up 1", "forward 6", "up 1", "down 3", "forward 3", "down 7", "down 4", "forward 2", "forward 8", "forward 7", "down 6", "up 1", "forward 8", "down 8", "forward 5", "down 4", "forward 2", "up 8", "down 5", "down 9", "forward 1", "down 8", "down 5", "forward 6", "down 1", "down 6", "up 5", "up 8", "forward 7", "forward 4", "down 5", "down 1", "forward 4", "up 9", "forward 9", "up 7", "down 4", "forward 8", "forward 5", "up 8", "down 9", "up 8", "forward 7", "down 8", "forward 4", "down 8", "up 7", "forward 4", "down 9", "forward 5", "up 7", "forward 5", "forward 9", "down 6", "up 6", "forward 2", "down 4", "down 8", "forward 9", "forward 4", "forward 6", "up 6", "down 6", "forward 6", "forward 7", "forward 5", "forward 3", "forward 7", "up 3", "down 6", "forward 2", "down 9", "up 8", "down 2", "forward 7", "down 9", "down 3", "up 9", "up 6", "down 6", "forward 8", "up 2", "up 1", "down 5", "forward 4", "down 2", "up 2", "forward 2", "forward 3", "up 5", "down 6", "forward 9", "up 3", "forward 6", "forward 4", "down 3", "up 9", "forward 1", "up 6", "down 9", "down 7", "forward 5", "down 2", "down 6", "forward 5", "forward 1", "forward 5", "forward 8", "up 2", "forward 9", "down 8", "forward 2", "down 6", "down 1", "down 9", "forward 6" ] :: Array String

type SubState
  = { depth :: Int
    , hPos :: Int
    , aim :: Int
    }

initState = { depth: 0, hPos: 0, aim: 0 } :: SubState

data Command
  = Forward Int
  | Up Int
  | Down Int

type UpdateFunc
  = SubState -> Command -> SubState

updateState :: UpdateFunc
updateState s c = case c of
  Forward x -> s { hPos = s.hPos + x }
  Up x -> s { depth = s.depth - x }
  Down x -> s { depth = s.depth + x }

updateState2 :: UpdateFunc
updateState2 s c = case c of
  Forward x -> s { hPos = s.hPos + x, depth = s.depth + (s.aim * x) }
  Up x -> s { aim = s.aim - x }
  Down x -> s { aim = s.aim + x }

parseCommand :: String -> Maybe Command
parseCommand s =
  let
    pair = split (Pattern " ") s
  in
    do
      command <- pair !! 0
      amount <- fromString =<< pair !! 1
      case command of
        "forward" -> Just (Forward amount)
        "up" -> Just (Up amount)
        "down" -> Just (Down amount)
        _ -> Nothing

foldm :: UpdateFunc -> SubState -> String -> SubState
foldm updateFunc s cs = case parseCommand cs of
  Just command -> updateFunc s command
  Nothing -> s

runProgram :: UpdateFunc -> Array String -> Int
runProgram updateFunc commands = final.depth * final.hPos
  where
  final = foldl (foldm updateFunc) initState commands

part1 = runProgram updateState :: Array String -> Int

part2 = runProgram updateState2 :: Array String -> Int

main :: Effect Unit
main = do
  log "🍝"
