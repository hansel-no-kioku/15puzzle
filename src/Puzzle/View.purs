module Puzzle.View
  ( PuzzleView
  , EventMove(..)
  , EventFinish(..)
  , newPuzzleView
  , setTilesView
  , moveTileView
  , setInteractiveView
  , updateTimeView
  , startView
  , finishView
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Phina (class Event, CountScene, CountSetting, DisplayScene, Duration, Label, SceneHandle, addChild, addChildB', animateB, build, call, countDefault, flare, getCenterPosB, getProp, getSizeB, make, newLabel, newRectangleShape, peek, popup', setPositionB, setProps, setText, toSceneHandle')
import Puzzle.Game.Tiles (Tile(..), TilePos, Tiles, getTilePos)
import Puzzle.View.Grids (Grids, calcTilePosition, getGridsUnit, newGrids)
import Puzzle.View.Style (finishMotion, style)
import Puzzle.View.TileShapes (TileShapes, moveTileShape, newTileShapes, setTileShapesPosition, sleepAllTileShapes, wakeUpTileShapes)
import Type.Prelude (SProxy(..))


type PuzzleView =
  { scene ∷ DisplayScene
  , grids ∷ Grids
  , timeLabel ∷ Label
  , shapes ∷ TileShapes
  }

data EventMove = EventMove
instance eventEventMove ∷ Event EventMove Int where
  event _ = "move"

data EventFinish = EventFinish
instance eventEventFinish ∷ Event EventFinish {score ∷ String} where
  event _ = "finish"


newPuzzleView ∷ DisplayScene → Effect PuzzleView
newPuzzleView scene = do
  _ ← setProps style.scene scene

  sceneWidth ← getProp (SProxy ∷ SProxy "width") scene

  timeLabel ← newLabel $ style.timeLabel {x: sceneWidth, y: 0.0}
  _ ← addChild timeLabel scene

  let
    tl = style.tilesLayout
    grids = newGrids sceneWidth tl.marginTop tl.marginSide
    tileWidth = getGridsUnit grids

  shapes ← newTileShapes tileWidth EventMove scene

  pure {scene, grids, timeLabel, shapes}


setTilesView ∷ Tiles → PuzzleView -> Effect Unit
setTilesView tiles view =
  setTileShapesPosition noToPosition view.shapes
  where
    noToPosition = calcTilePosition view.grids <<< getTilePos tiles <<< Tile


moveTileView ∷ Tile → TilePos → PuzzleView → Effect Unit
moveTileView (Tile no) tilePos view =
  let pos = calcTilePosition view.grids tilePos
  in  moveTileShape (no - 1) pos view.shapes
moveTileView Null _ _ = pure unit


setInteractiveView ∷ Tiles → PuzzleView → Effect Unit
setInteractiveView tiles view = do
  sleepAllTileShapes view.shapes
  wakeUpTileShapes (mapMaybe getShapeNo tiles) view.shapes
  where
    getShapeNo (Tile no) = Just (no - 1)
    getShapeNo Null = Nothing


updateTimeView ∷ Duration → PuzzleView → Effect Unit
updateTimeView time view = void $ setText (style.timeText time) view.timeLabel


startView ∷ PuzzleView → Effect Unit
startView view = void $ popup' countScene {count: countDefault} view.scene
  where
    countScene ∷ SceneHandle CountScene (count ∷ CountSetting) ()
    countScene = toSceneHandle' \_ _ → setProps style.countScene


finishView ∷ Duration → PuzzleView → Effect Unit
finishView time view = do
  sleepAllTileShapes view.shapes

  void $ flip build view.scene do
    scene ← peek
    pos ← getCenterPosB
    size ← getSizeB

    addChildB' $ newRectangleShape $ style.finishMask pos size

    addChildB' $ make (newLabel style.finishLabel) do
      setPositionB pos
      animateB do
        finishMotion
        call \l →
          const l <$> flare EventFinish {score: style.timeText time} scene
