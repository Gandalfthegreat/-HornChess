module Lib where

-- main library of Brick

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Border (borderWithLabel, vBorder)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import Graphics.Vty.Input.Events

data St = St
  { board :: String
  }

initialState =
  St
    { board = "X X O"
    }

app :: App St e ()
app =
  App
    { appDraw = draw, -- UI function
      appChooseCursor = const . const Nothing, -- don't need cursor
      appHandleEvent = handleEvent, -- handle event to change state
      appStartEvent = return,
      appAttrMap = const $ attrMap mempty [] -- add style to UI
    }

-- buildInitialState :: IO TuiState
-- buildInitialState = pure
draw :: St -> [Widget ()]
draw st = [ui]
  where
    ui =
      vBox
        [ str "HornChess",
          str "(Press Esc to quit)"
        ]

handleEvent :: St -> BrickEvent () e -> EventM () (Next St)
handleEvent s e =
  case e of
    VtyEvent vtye ->
      --- press q t exit the program
      case vtye of
        EvKey KEsc [] -> halt s
        _ -> continue s
    _ -> continue s
