module Main where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Control.Monad.Eff.Timer (TIMER, setInterval)
import Data.Array (zipWith)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (Arc, CANVAS, arc, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, setLineWidth, setStrokeStyle, stroke)
import Math as Math
import Prelude (Unit, bind, map, negate, void, ($), (*), (+), (/), (==), (||))
import Types (Color(..), Angle, Circle, Radius, color)

type CanvasEff a = forall e. Eff (canvas :: CANVAS | e) a

speed :: Number
speed = Math.pi / 32.0

width :: Number
width = 500.0

height :: Number
height = 500.0

radiuss :: Array Radius
radiuss = [ 50.0, 75.0, 100.0, 125.0 ]

startingAngles :: Array Angle
startingAngles = [ 0.0, Math.pi * 0.5, Math.pi, Math.pi * 1.5 ]

angleRadiuss :: Array { angle :: Angle, radius :: Radius }
angleRadiuss = zipWith (\angle radius -> { angle, radius }) startingAngles radiuss

updateCircles :: Array Circle -> Array Circle
updateCircles = map move
  where
    move c = if c.color == Yellow || c.color == Green
      then c { arc {start = c.arc.start + (-1.0 * speed), end = c.arc.end + (-1.0 * speed) }}
      else c { arc {start = c.arc.start + speed, end = c.arc.end + speed }}

arcs :: Array Arc
arcs = map (\a ->
    { x: width / 2.0
    , y: width / 2.0
    , r: a.radius
    , start: a.angle
    , end: a.angle + Math.pi
    }
  ) angleRadiuss

colors :: Array Color
colors = [ Blue, Red, Green, Yellow ]

circles :: Array Circle
circles = zipWith (\color arc -> { color, arc }) colors arcs

main :: forall e. Partial => Eff ( ref :: REF, timer :: TIMER, canvas :: CANVAS | e ) Unit
main = void $ do
  Just canvasElem <- getCanvasElementById "canvas"
  ctx             <- getContext2D canvasElem
  circlesR        <- newRef circles

  setInterval 50 $ void do
    clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}
    circles' <- readRef circlesR
    modifyRef circlesR updateCircles
    foreachE circles' \c -> do
      void $ drawCircle c ctx
  where
    drawCircle circle ctx = do
      beginPath      ctx
      arc            ctx                  circle.arc
      setLineWidth   3.0                  ctx
      setStrokeStyle (color circle.color) ctx
      stroke         ctx
      closePath      ctx
