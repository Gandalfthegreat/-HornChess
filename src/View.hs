{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module View where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Widget,
    attrMap,
    continue,
    customMain,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
  )
import Brick.BChan (newBChan, writeBChan)
import Brick.Main
import Brick.Types
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import Control.Concurrent (MVar, tryPutMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Monoid
import Data.Sequence (Seq, fromList)
import Graphics.Vty (black)
import qualified Graphics.Vty as V
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Linear.V2 (V2 (..), _x, _y)
import Model

---------------------------------------------------------constant------------------------------------------------------------------
type Name = ()

cw :: Widget Name
cw = str "*" -- pixel is 2 spaces

height = 58

width = 64

--------------------------------------------------draw function-----------------------------------------------------------------------------------------------------
draw :: St -> [Widget Name]
draw st = [C.center (drawGrid st)]

drawGrid :: St -> Widget Name
drawGrid st =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "HornChess") $ vBox rows
  where
    -- draw all the cells in the grid.
    -- a cell represent  disk, peg or Empty.
    rows = [hBox $ cellsInRow r | r <- [height -1, height -2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width -1]]
    drawCoord = drawCell . cellAt

    -- cellAt c
    --   | otherwise = Empty
    cellAt c -- give a coordinate, return a state of pixel
      | isPartOfBoard c = Board
      | isPartOfWhite c = WhiteChess
      | isPartOfBlack c = BlackChess
      | otherwise = Background

-- draw a cell
drawCell :: Pixel -> Widget Name
drawCell Board = withAttr boardAttr cw
drawCell Background = withAttr bgAttr cw
drawCell WhiteChess = withAttr whiteAttr cw
drawCell BlackChess = withAttr blackAttr cw

--------------------------------------------------get cell to render -------------------------------------
isPartOfBoard c = c `elem` pegBase

isPartOfBlack c = c `elem` blackList

isPartOfWhite c = c `elem` whiteList

whiteList = [V2 01 02]

blackList = [V2 15 18, V2 19 30]

--------------------------------------------------hard code chess-------------------------------------
pegBase = horizontalLine ++ zone1 ++ zone2 ++ zone3 ++ zone4 ++ zone5 ++ zone6

horizontalLine =
  fmap (\x -> V2 x 01) [5 .. 42]
    ++ fmap (\x -> V2 x 13) [9 .. 43]
    ++ fmap (\x -> V2 x 25) [13 .. 45]
    ++ fmap (\x -> V2 x 35) [18 .. 46]
    ++ fmap (\x -> V2 x 43) [26 .. 49]
    ++ fmap (\x -> V2 x 49) [35 .. 53]
    ++ fmap (\x -> V2 x 56) [53 .. 58]

-- zone is from bottom to up
zone1 =
  fmap (\x -> V2 x 2) ([5, 38 .. 40] ++ [42])
    ++ fmap (\x -> V2 x 3) ([5, 35 .. 37] ++ [42])
    ++ fmap (\x -> V2 x 4) ([6, 32 .. 34] ++ [42])
    ++ fmap (\x -> V2 x 5) ([6, 29 .. 31] ++ [42])
    ++ fmap (\x -> V2 x 6) ([7, 26 .. 28] ++ [42])
    ++ fmap (\x -> V2 x 7) ([7, 23 .. 25] ++ [43])
    ++ fmap (\x -> V2 x 8) ([7, 20 .. 22] ++ [43])
    ++ fmap (\x -> V2 x 9) ([8, 17 .. 19] ++ [43])
    ++ fmap (\x -> V2 x 10) ([8, 14 .. 16] ++ [43])
    ++ fmap (\x -> V2 x 11) ([9, 11 .. 13] ++ [43])
    ++ fmap (\x -> V2 x 12) ([9, 10] ++ [43])

zone2 =
  fmap (\x -> V2 x 14) ([9, 40, 41, 43])
    ++ fmap (\x -> V2 x 15) ([9, 37 .. 39] ++ [43])
    ++ fmap (\x -> V2 x 16) ([10, 35, 36, 43])
    ++ fmap (\x -> V2 x 17) ([10, 32 .. 34] ++ [44])
    ++ fmap (\x -> V2 x 18) ([10, 30, 31, 44])
    ++ fmap (\x -> V2 x 19) ([11, 27 .. 29] ++ [44])
    ++ fmap (\x -> V2 x 20) ([11, 25, 26, 44])
    ++ fmap (\x -> V2 x 21) ([12, 22 .. 24] ++ [44])
    ++ fmap (\x -> V2 x 22) ([12, 20, 21, 44])
    ++ fmap (\x -> V2 x 23) ([13, 17 .. 19] ++ [43])
    ++ fmap (\x -> V2 x 24) ([13, 15, 16, 45])

-- ....................................................
zone3 =
  fmap (\x -> V2 x 27) ([14, 39 .. 40] ++ [45])
    ++ fmap (\x -> V2 x 28) ([14, 36 .. 38] ++ [45])
    ++ fmap (\x -> V2 x 29) ([15, 33 .. 35] ++ [45])
    ++ fmap (\x -> V2 x 30) ([15, 31, 32] ++ [45])
    ++ fmap (\x -> V2 x 31) ([16, 28 .. 30] ++ [46])
    ++ fmap (\x -> V2 x 32) ([16, 25 .. 27] ++ [46])
    ++ fmap (\x -> V2 x 33) ([17, 23, 24] ++ [46])
    ++ fmap (\x -> V2 x 34) ([17, 20 .. 22] ++ [46])
    ++ fmap (\x -> V2 x 35) ([15, 33 .. 35] ++ [45])

zone4 = []

zone5 = []

zone6 = []

bgAttr, blackAttr, whiteAttr, boardAttr :: AttrName
bgAttr = "bgAttr"
blackAttr = "blackAttr"
whiteAttr = "whiteAttr"
boardAttr = "boardAttr"

darkBlack = V.rgbColor 0 0 0

grey = V.rgbColor 80 50 50

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (bgAttr, grey `on` grey),
      (blackAttr, V.yellow `on` V.yellow),
      (whiteAttr, V.white `on` V.white),
      (boardAttr, darkBlack `on` darkBlack)
      -- ,(emptyAttr,  V.black `on` V.black )
    ]
