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
import qualified Graphics.Vty as V
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Linear.V2 (V2 (..), _x, _y)
import Model

type Name = ()

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
    cellAt c = Empty

--  isCellPartOfDiskSet c diskSet =
--      any (==True) $ fmap isPartOfDisk [0,1,2] -- for each disk check
--    where
--      isPartOfDisk  i = c `elem` ( diskSet M.! i)
--  isCellPartOfPegSet c =  c `elem` towerPegSet

-- draw a cell
drawCell :: Chess -> Widget Name
-- drawCell WHITE = withAttr boardAttr cw
-- drawCell WHITE    = withAttr blackAttr cw
-- drawCell Empty      = withAttr whiteAttr cw
drawCell Empty = withAttr bgAttr cw
drawCell _ = withAttr bgAttr cw

-- pixel is 2 spaces
cw :: Widget Name
cw = str " "

height = 58

width = 64

-- ....................................................
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
