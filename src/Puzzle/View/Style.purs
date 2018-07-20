module Puzzle.View.Style
  ( style
  , tileColors
  , tileMotion
  , finishMotion
  ) where

import Prelude

import Data.Array (take)
import Data.Number.Format (fixed, toStringWith)
import Phina (Color, Duration, Label, Position, RectangleShape, Tween, color, easeInOutQuad, easeOutBounce, moveTo, msec, nullColor, scaleTo, sec, toSec, wait)

style =
  { game:
      { title: "15 puzzle"
      , message: "15 puzzle"
      , hashtags: "phina_js,game,purescript"
      }

  , scene:
      { backgroundColor: color "#666"
      }

  , countScene:
      { backgroundColor: color "rgba(0, 0, 0, 0.5)"
      }

  , timeLabel: \pos →
      { x: pos.x - 80.0
      , y: pos.y + 64.0
      , text: timeText zero
      , fontSize: 52.0
      , fontFamily: "monospace"
      , fontWeight: "bold"
      , align: "right"
      , fill: color "white"
      , stroke: color "black"
      , strokeWidth:2.0
      }

  , timeText:
      timeText

  , tilesLayout:
      { marginTop: 160.0
      , marginSide: 16.0
      }

  , tileShape: \size col →
      { width: size - 12.0
      , height: size - 12.0
      , cornerRadius: 16.0
      , fill: col
      , stroke: color "white"
      , strokeWidth: 4.0
      , shadow: color "black"
      , shadowBlur: 12.0
      , padding: 12.0
      }

  , tileLabel:
      { text: _
      , fontSize: 64.0
      , fontWeight: "bold"
      , fill: color "white"
      , stroke: color "black"
      , strokeWidth: 2.0
      , shadow: color "black"
      , shadowBlur: 8.0
      }

  , finishMask: \pos size →
    { x: pos.x
    , y: pos.y
    , width: size.width
    , height: size.height
    , padding: 0.0
    , fill: color "rgba(0, 0, 0, 0.5)"
    , stroke: nullColor
    }

  , finishLabel:
      { text: "Finish!"
      , scaleX: 0.1
      , scaleY: 0.1
      , fontSize: 128.0
      , fontWeight: "bold"
      , fill: color "white"
      , stroke: color "black"
      , strokeWidth: 2.0
      , shadow: color "black"
      , shadowBlur: 8.0
      }
  }

tileColors ∷ Array Color
tileColors = take 15 $ (\r g b → color $ "#" <> r <> g <> b) <$> rl <*> gl <*> bl
  where
    rl = ["b"]
    gl = ["e", "d", "c", "b"]
    bl = gl

timeText ∷ Duration → String
timeText = (_ <> " sec") <<< toStringWith (fixed 2) <<< toSec

tileMotion ∷ Position → Tween RectangleShape
tileMotion pos = moveTo pos.x pos.y (msec 200) easeInOutQuad

finishMotion ∷ Tween Label
finishMotion = do
  scaleTo 1.0 (sec 2) easeOutBounce
  wait $ sec 1
