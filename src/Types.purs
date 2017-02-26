module Types where

import Graphics.Canvas (Arc)
import Prelude (class Show, show)

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

color :: Color -> String
color = show
