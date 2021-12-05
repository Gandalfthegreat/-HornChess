module Main where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import Control.Monad (void)
import Graphics.Vty.Input.Events
import Lib

main :: IO ()
main = do
  void $ defaultMain app initialState
