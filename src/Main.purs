module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Graphics.Canvas
import Prelude

type CanvasEff a = forall e. Eff (canvas :: CANVAS | e) a

main :: Partial => CanvasEff Unit
main = do
  Just canvasElem <- getCanvasElementById "canvas"
  ctx             <- getContext2D canvasElem
  pure unit
