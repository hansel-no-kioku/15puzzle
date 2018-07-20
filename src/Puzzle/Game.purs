module Puzzle.Game
  ( newPuzzleGame
  , updateTime
  , moveTileByNo
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref, modify, new, read, write)
import Phina (Duration)
import Puzzle.Game.Tiles (Tile(..), Tiles, getMovableTiles, getTilePos, initialTiles, isFinish, moveTile, shuffleTiles)
import Puzzle.View (PuzzleView, finishView, moveTileView, setInteractiveView, setTilesView, startView, updateTimeView)


shuffleTimes ∷ Int
shuffleTimes = 100

type PuzzleGame =
  { tilesRef ∷ Ref Tiles
  , timeRef ∷ Ref Duration
  , isFinish ∷ Ref Boolean
  , view ∷ PuzzleView
  }


newPuzzleGame ∷ PuzzleView → Effect PuzzleGame
newPuzzleGame view = do
  tiles ← shuffleTiles shuffleTimes initialTiles
  tilesRef ← new tiles
  timeRef ← new zero
  isFinish ← new false

  setTilesView tiles view
  setInteractiveView (getMovableTiles tiles) view

  startView view

  pure {tilesRef, timeRef, isFinish, view}


updateTime ∷ Duration → PuzzleGame → Effect Unit
updateTime delta game = do
  f ← read game.isFinish
  unless f do
    time ← modify (_ + delta) game.timeRef
    updateTimeView time game.view


moveTileByNo ∷ Int → PuzzleGame → Effect Unit
moveTileByNo no game = do
  tiles ← read game.tilesRef
  let
    from = getTilePos tiles $ Tile no
    to = getTilePos tiles Null
    tiles' = moveTile tiles {from, to}
  write tiles' game.tilesRef
  moveTileView (Tile no) to game.view
  if isFinish tiles'
    then do
      _ ← write true game.isFinish
      time ← read game.timeRef
      finishView time game.view
    else
      setInteractiveView (getMovableTiles tiles') game.view
