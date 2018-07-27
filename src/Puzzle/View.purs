module Puzzle.View
  ( puzzleView
  , EventMove(..)
  , EventFinish(..)
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.FSM (Machine, machine)
import Phina (class Event, CountScene, CountSetting, DisplayScene, Duration, Label, SceneHandle, addChild, addChildB', animateB, build, call, countDefault, flare, getCenterPosB, getProp, getSizeB, make, newLabel, newRectangleShape, peek, popup', setPositionB, setProps, setText, toSceneHandle')
import Puzzle.Game.Tiles as T
import Puzzle.View.Grids (Grids, calcTilePosition, getGridsUnit, newGrids)
import Puzzle.View.Message as VM
import Puzzle.View.Style (finishMotion, style)
import Puzzle.View.TileShapes (TileShapes, moveTileShape, newTileShapes, setTileShapesPosition, sleepAllTileShapes, wakeUpTileShapes)
import Type.Prelude (SProxy(..))


type ViewState =
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


puzzleView ∷ DisplayScene → Effect (Machine VM.Message Unit)
puzzleView scene = do
  state ← initialState scene
  machine (\i s → viewFunc i s $> Tuple unit s) state VM.Init unit


initialState ∷ DisplayScene → Effect ViewState
initialState scene = do
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


viewFunc ∷ VM.Message → ViewState → Effect Unit
viewFunc VM.Init = \_ → pure unit
viewFunc VM.Nothing = \_ → pure unit
viewFunc (VM.InitialSet tiles mTiles) = initialSet tiles mTiles
viewFunc (VM.UpdateTime time) = updateTime time
viewFunc (VM.MoveTile no pos mTiles) = moveTile no pos mTiles
viewFunc (VM.MoveTileFinish no pos time) = moveTileFinish no pos time


initialSet ∷ T.Tiles → T.Tiles → ViewState → Effect Unit
initialSet tiles mTiles state = do
  setTileShapesPosition noToPosition state.shapes
  setInteractiveView mTiles state.shapes
  countDown state.scene
  where
    noToPosition = calcTilePosition state.grids <<< T.getTilePos tiles <<< T.Tile


updateTime ∷ Duration → ViewState → Effect Unit
updateTime time state = void $ setText (style.timeText time) state.timeLabel


moveTile ∷ Int → T.TilePos → T.Tiles → ViewState → Effect Unit
moveTile no pos mTiles state = do
  moveTileByNo no pos state
  setInteractiveView mTiles state.shapes


moveTileFinish ∷ Int → T.TilePos → Duration → ViewState → Effect Unit
moveTileFinish no pos time state = do
  moveTileByNo no pos state
  finishView time state


moveTileByNo ∷ Int → T.TilePos → ViewState → Effect Unit
moveTileByNo no tilePos state = do
  let pos = calcTilePosition state.grids tilePos
  moveTileShape (no - 1) pos state.shapes


setInteractiveView ∷ T.Tiles → TileShapes → Effect Unit
setInteractiveView tiles shapes = do
  sleepAllTileShapes shapes
  wakeUpTileShapes (mapMaybe getShapeNo tiles) shapes
  where
    getShapeNo (T.Tile no) = Just (no - 1)
    getShapeNo T.Null = Nothing


countDown ∷ DisplayScene → Effect Unit
countDown scene = void $ popup' countScene {count: countDefault} scene
  where
    countScene ∷ SceneHandle CountScene (count ∷ CountSetting) ()
    countScene = toSceneHandle' \_ _ → setProps style.countScene


finishView ∷ Duration → ViewState → Effect Unit
finishView time state = do
  sleepAllTileShapes state.shapes

  void $ flip build state.scene do
    scene ← peek
    pos ← getCenterPosB
    size ← getSizeB

    addChildB' $ newRectangleShape $ style.finishMask pos size

    addChildB' $ make (newLabel style.finishLabel) do
      setPositionB pos
      animateB do
        finishMotion
        call \l → flare EventFinish {score: style.timeText time} scene $> l
