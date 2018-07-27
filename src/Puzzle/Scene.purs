module Puzzle.Scene
  ( setupScene
  ) where

import Prelude

import Effect.FSM (send, ($$))
import Phina (DisplayScene, SetupScene, build, getProp, onB, setUpdaterB)
import Puzzle.Game (puzzleGame)
import Puzzle.Game.Message (Message(..))
import Puzzle.View (EventFinish(..), EventMove(..), puzzleView)
import Type.Prelude (SProxy(..))


setupScene ∷ SetupScene DisplayScene () (score ∷ String)
setupScene _ exit scene = do
  game ← puzzleGame
  view ← puzzleView scene
  gv ← game $$ view

  scene # build do
    setUpdaterB \app s → do
      delta ← getProp (SProxy ∷ SProxy "deltaTime") app
      send (UpdateTime delta) gv
    onB EventMove \no s → send (MoveTile no) gv
    onB EventFinish \result s → exit result s
