module Main where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import Control.Monad (void)
import Lib
import Model

main :: IO ()
main = do
  void $ defaultMain app initialState
