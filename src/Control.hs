module Control where

import Brick.Main
import Brick.Types
import Graphics.Vty.Input.Events
import Model

handleEvent :: St -> BrickEvent () e -> EventM () (Next St)
handleEvent s e =
  case e of
    VtyEvent vtye ->
      --- press q t exit the program
      case vtye of
        EvKey KEsc [] -> halt s
        _ -> continue s
    _ -> continue s
