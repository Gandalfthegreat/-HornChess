module View where

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Model

draw :: PlayState -> [Widget ()]
draw st = [ui]
  where
    ui =
      vBox
        [ str "HornChess",
          str "(Press Esc to quit)"
        ]
