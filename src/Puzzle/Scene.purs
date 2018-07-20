module Puzzle.Scene
  ( setupScene
  ) where

import Prelude

import Phina (DisplayScene, SetupScene, build, getProp, onB, setUpdaterB)
import Puzzle.Game (moveTileByNo, newPuzzleGame, updateTime)
import Puzzle.View (EventFinish(..), EventMove(..), newPuzzleView)
import Type.Prelude (SProxy(..))


setupScene ∷ SetupScene DisplayScene () (score ∷ String)
setupScene _ exit scene = do
  view ← newPuzzleView scene
  game ← newPuzzleGame view

  scene # build do
    setUpdaterB \app s → do
      delta ← getProp (SProxy ∷ SProxy "deltaTime") app
      updateTime delta game
    onB EventMove \no s → const s <$> moveTileByNo no game
    onB EventFinish \result s → exit result s
