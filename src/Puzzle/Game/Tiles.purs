module Puzzle.Game.Tiles
  ( Tile(..)
  , Tiles
  , TilePos
  , TileMove
  , initialTiles
  , shuffleTiles
  , getTilePos
  , getMovableTiles
  , moveTile
  , isFinish
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (elemIndex, filter, length, mapMaybe, snoc, updateAt, (!!), (..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Random (randomInt)


data Tile = Tile Int | Null

derive instance eqTile ∷ Eq Tile
derive instance ordTile ∷ Ord Tile

type Tiles = Array Tile
type TilePos = {x ∷ Int, y ∷ Int}
type TileMove = {from ∷ TilePos, to ∷ TilePos}

data Move = Up | Right | Down | Left


initialTiles ∷ Tiles
initialTiles = (Tile <$> 1..15) `snoc` Null


data Shuffle n p t = Shuffle Int TilePos Tiles

shuffleTiles ∷ Int → Tiles → Effect Tiles
shuffleTiles num tiles =
  tailRecM go $ Shuffle num (getTilePos tiles Null) tiles

  where
    go (Shuffle 0 pre tiles') = pure $ Done tiles'
    go (Shuffle n pre tiles') = do
      let
        tileMoves = filter (\m → m.from /= pre) $ getTileMoves tiles'
        len = length tileMoves
      i ← randomInt 0 $ len - 1
      case tileMoves !! i of
        Just tileMove →
          let tiles'' = moveTile tiles' tileMove
          in  pure $ Loop $ Shuffle (n - 1) tileMove.to tiles''
        Nothing → pure $ Done tiles'

    getTileMoves tiles' =
      let to = getTilePos tiles' Null
      in  mapMaybe ({from: _, to} <$> _)
            $ nextTilePos to <$> [Up, Right, Down, Left]


getTilePos ∷ Tiles → Tile → TilePos
getTilePos tiles t = indexToPos $ fromMaybe 0 $ elemIndex t tiles


getMovableTiles ∷ Tiles → Array Tile
getMovableTiles tiles =
  let to = getTilePos tiles Null
  in  mapMaybe (_ >>= \f → tiles !! posToIndex f)
        $ nextTilePos to <$> [Up, Right, Down, Left]


moveTile ∷ Tiles → TileMove → Tiles
moveTile tiles move =
  let from = posToIndex move.from
      to = posToIndex move.to
  in
    fromMaybe tiles do
      a ← tiles !! from
      b ← tiles !! to
      tiles' ← updateAt from b tiles
      updateAt to a tiles'


isFinish ∷ Tiles → Boolean
isFinish = _.ret <<< foldl f {ret: true, tile: Tile 0}
  where
    f pre tile = {ret: pre.ret && pre.tile < tile, tile}

-- private

nextTilePos ∷ TilePos → Move → Maybe TilePos
nextTilePos pos Up    | pos.y > 0 = Just $ pos {y = pos.y - 1}
nextTilePos pos Right | pos.x < 3 = Just $ pos {x = pos.x + 1}
nextTilePos pos Down  | pos.y < 3 = Just $ pos {y = pos.y + 1}
nextTilePos pos Left  | pos.x > 0 = Just $ pos {x = pos.x - 1}
nextTilePos _ _ = Nothing


indexToPos ∷ Int → TilePos
indexToPos i = {x: i `mod` 4, y: i / 4}

posToIndex ∷ TilePos → Int
posToIndex pos = pos.y * 4 + pos.x
