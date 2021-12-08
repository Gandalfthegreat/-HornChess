module Lib where

-- main library of Brick
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Border (borderWithLabel, vBorder)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import Control
import Graphics.Vty.Input.Events
import Model
import View

app :: App St e ()
app =
  App
    { appDraw = draw, -- UI function
      appChooseCursor = const . const Nothing, -- don't need cursor
      appHandleEvent = handleEvent, -- handle event to change state
      appStartEvent = return,
      appAttrMap = const theMap -- add style to UI
    }
