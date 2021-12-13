module Geometry where

import Prelude

newtype Point
  = Point
  { x :: Int
  , y :: Int
  }

instance showPoint :: Show Point where
  show (Point { x, y }) = show x <> "," <> show y

newtype Line
  = Line
  { start :: Point
  , stop :: Point
  }

instance showLine :: Show Line where
  show (Line { start, stop }) = show start <> " -> " <> show stop

newtype Dimensions
  = Dimensions
  { width :: Int
  , height :: Int
  }

instance showDimensions :: Show Dimensions where
  show (Dimensions { width, height }) = show width <> "x" <> show height

newtype Rect
  = Rect
  { origin :: Point
  , size :: Dimensions
  }

instance showRect :: Show Rect where
  show (Rect { origin, size }) = "( " <> show origin <> " " <> show size <> " )"
