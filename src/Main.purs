module Main where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (zipWith)
import Data.Maybe (Maybe(..))
import Graphics.Canvas
import Math as Math
import Prelude
import Types (Color(..), Angle, Circle, Radius, color)

type CanvasEff a = forall e. Eff (canvas :: CANVAS | e) a

width :: Number
width = 500.0

height :: Number
height = 500.0

startingAngles :: Array Angle
startingAngles = [ 0.0, Math.pi * 0.5, Math.pi, Math.pi * 1.5 ]

colors :: Array Color
colors = [ Blue, Red, Green, Yellow ]

radiuss :: Array Radius
radiuss = [ 50.0, 75.0, 100.0, 125.0 ]

angleRadiuss :: Array { angle :: Angle, radius :: Radius }
angleRadiuss = zipWith (\a r -> { angle: a, radius: r }) startingAngles radiuss

arcs :: Array Arc
arcs = map (\a ->
    { x: width / 2.0
    , y: width / 2.0
    , r: a.radius
    , start: a.angle
    , end: a.angle + Math.pi
    }
  ) angleRadiuss

circles :: Array Circle
circles = zipWith (\color arc -> { color, arc}) colors arcs

drawCircle :: Circle -> Context2D -> CanvasEff Context2D
drawCircle circle ctx = do
  beginPath      ctx
  arc            ctx                  circle.arc
  setLineWidth   3.0                  ctx
  setStrokeStyle (color circle.color) ctx
  stroke         ctx
  closePath      ctx

main :: Partial => CanvasEff Unit
main = do
  Just canvasElem <- getCanvasElementById "canvas"
  ctx             <- getContext2D canvasElem

  foreachE circles \c -> do
    void $ drawCircle c ctx
