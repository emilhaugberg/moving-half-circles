module Types where

import Graphics.Canvas (Arc)
import Prelude (class Eq, class Show, show)

type Circle = { color :: Color, arc :: Arc }
type Angle  = Number
type Radius = Number

data Color = Blue | Green | Yellow | Red | Orange

instance showColor :: Show Color where
  show Blue   = "#2196F3"
  show Green  = "#4CAF50"
  show Yellow = "#FFEB3B"
  show Red    = "#F44336"
  show Orange = "#FF9800"

instance eqColor :: Eq Color where
  eq Blue   Blue   = true
  eq Green  Green  = true
  eq Yellow Yellow = true
  eq Red    Red    = true
  eq Orange Orange = true
  eq _      _      = false

color :: Color -> String
color = show
