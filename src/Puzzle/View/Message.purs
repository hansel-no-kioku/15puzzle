module Puzzle.View.Message
  ( Message(..)
  ) where

import Phina (Duration)
import Puzzle.Game.Tiles (Tiles, TilePos)

data Message  = Init
              | Nothing
              | InitialSet Tiles Tiles
              | UpdateTime Duration
              | MoveTile Int TilePos Tiles
              | MoveTileFinish Int TilePos Duration
