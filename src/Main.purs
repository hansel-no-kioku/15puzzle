module Main where

import Prelude

import Effect (Effect)
import Phina (GameScenes(..), StartScene(..), newGame, runGame)
import Puzzle.Scene (setupScene)
import Puzzle.View.Style (style)

main ∷ Effect Unit
main = do
  game ← newGame style.game $ SceneListDefault Splash setupScene
  runGame game
