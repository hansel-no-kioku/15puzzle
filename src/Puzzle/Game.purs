module Puzzle.Game
  ( puzzleGame
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.FSM (Machine, machine)
import Phina (Duration)
import Puzzle.Game.Message as GM
import Puzzle.Game.Tiles (Tile(..), Tiles, getMovableTiles, getTilePos, initialTiles, isFinish, moveTile, shuffleTiles)
import Puzzle.View.Message as VM


shuffleTimes ∷ Int
shuffleTimes = 100

type GameState =
  { tiles ∷ Tiles
  , time ∷ Duration
  , isFinish ∷ Boolean
  }

type EffectEmit = Effect (Tuple VM.Message GameState)


puzzleGame ∷ Effect (Machine GM.Message VM.Message)
puzzleGame = do
  machine gameFunc initialState GM.Init VM.Nothing


initialState ∷ GameState
initialState =
  { tiles: mempty
  , time: zero
  , isFinish: false
  }


gameFunc ∷ GM.Message → GameState → EffectEmit
gameFunc GM.Init s = initTiles s
gameFunc (GM.UpdateTime time) s = updateTime time s
gameFunc (GM.MoveTile no) s = moveTileByNo no s


initTiles ∷ GameState → EffectEmit
initTiles state = ado
  tiles ← shuffleTiles shuffleTimes initialTiles
  in Tuple
    (VM.InitialSet tiles (getMovableTiles tiles))
    (state {tiles = tiles})


updateTime ∷ Duration → GameState → EffectEmit
updateTime delta state =
  pure if state.isFinish
          then Tuple VM.Nothing state
          else
            let time = state.time + delta
            in Tuple (VM.UpdateTime time) (state {time = time})


moveTileByNo ∷ Int → GameState → EffectEmit
moveTileByNo no state = ado
  let
    from = getTilePos state.tiles $ Tile no
    to = getTilePos state.tiles Null
    tiles' = moveTile state.tiles {from, to}
    isFinish' = isFinish tiles'
    output = if isFinish'
      then VM.MoveTileFinish no to state.time
      else VM.MoveTile no to $ getMovableTiles tiles'
    state' = state {tiles = tiles', isFinish = isFinish'}
  in Tuple output state'
