module Day5 where

import Prelude
import Data.Array.NonEmpty (index)
import Data.Either (hush)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromJust)
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)

newtype Point
  = Point
  { x :: Int
  , y :: Int
  }

instance Show Point where
  show (Point { x, y }) = show x <> "," <> show y

newtype Line
  = Line
  { start :: Point
  , stop :: Point
  }

instance Show Line where
  show (Line { start, stop }) = show start <> " -> " <> show stop

linePattern :: Regex
linePattern = unsafePartial $ fromJust $ hush $ regex "(.*),(.*) -> (.*),(.*)" noFlags

parseLine :: String -> Maybe Line
parseLine string = do
  m <- match linePattern string
  x1 <- index m 1 # join >>= fromString
  y1 <- index m 2 # join >>= fromString
  x2 <- index m 3 # join >>= fromString
  y2 <- index m 4 # join >>= fromString
  pure $ Line { start: Point { x: x1, y: y1 }, stop: Point { x: x2, y: y2 } }
