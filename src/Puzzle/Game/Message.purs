module Puzzle.Game.Message
  ( Message(..)
  ) where

import Phina (Duration)

data Message  = Init
              | UpdateTime Duration
              | MoveTile Int
