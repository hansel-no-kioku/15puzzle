module Puzzle.View.Grids
  ( Grids
  , newGrids
  , calcTilePosition
  , getGridsUnit
  ) where

import Prelude

import Phina (Grid, Position, getSpan, getUnit, newGrid)
import Puzzle.Game.Tiles (TilePos)


type Grids = {x ∷ Grid, y ∷ Grid}


newGrids ∷ Number → Number → Number → Grids
newGrids width marginTop marginSide =
  let width' = width - marginSide * 2.0
  in  { x: newGrid width' 4 false $ width / 8.0 + marginSide
      , y: newGrid width' 4 false $ width / 8.0 + marginTop
      }


calcTilePosition ∷ Grids → TilePos → Position
calcTilePosition grids pos =
  { x: getSpan pos.x grids.x
  , y: getSpan pos.y grids.y
  }


getGridsUnit ∷ Grids → Number
getGridsUnit = getUnit <<< _.x
