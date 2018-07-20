module Puzzle.View.TileShapes
  ( TileShapes
  , newTileShapes
  , setTileShapesPosition
  , moveTileShape
  , sleepAllTileShapes
  , wakeUpTileShapes
  ) where

import Prelude

import Data.Array (mapMaybe, zipWithA, (!!), (..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Phina (class Container, class Event, class EventDispatcher, Position, RectangleShape, addChild, animate, flare, newLabel, newRectangleShape, onPointEnd, setInteractive, setPosition)
import Puzzle.View.Style (style, tileColors, tileMotion)


type TileShapes = Array RectangleShape


newTileShapes
  ∷ ∀ e c
   . Event e Int
  ⇒ Container c
  ⇒ EventDispatcher c
  ⇒ Number
  → e
  → c
  → Effect TileShapes
newTileShapes size event container =
  zipWithA makeShape (1..15) tileColors
  where
    makeShape no color = do
      shape ← newRectangleShape $ style.tileShape size color
      label ← newLabel $ style.tileLabel $ show no
      _ ← addChild label shape
      _ ← addChild shape container
      onPointEnd (\_ _ → flare event no container) shape

setTileShapesPosition ∷ (Int → Position) → TileShapes -> Effect Unit
setTileShapesPosition noToPosition shapes =
  void $ zipWithA setShapePosition (1..15) shapes
  where
    setShapePosition no shape = setPosition (noToPosition no) shape

moveTileShape ∷ Int → Position → TileShapes → Effect Unit
moveTileShape i pos shapes =
  case shapes !! i of
    Just shape → void $ animate (tileMotion pos) shape
    _          → pure unit

sleepAllTileShapes ∷ TileShapes → Effect Unit
sleepAllTileShapes = traverse_ $ setInteractive false

wakeUpTileShapes ∷ Array Int → TileShapes → Effect Unit
wakeUpTileShapes nos shapes = do
  traverse_ (setInteractive true) $ mapMaybe (shapes !! _) nos
