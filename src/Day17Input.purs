module Day17Input where

import Prelude
import Geometry (Point(..), Rect, fromPoints)

-- | target area: x=20..30, y=-10..-5
testInput :: Rect
testInput = fromPoints (Point { x: 20, y: -10 }) (Point { x: 30, y: -5 })

-- | target area: x=60..94, y=-171..-136
realInput :: Rect
realInput = fromPoints (Point { x: 60, y: -171 }) (Point { x: 94, y: -136 })
