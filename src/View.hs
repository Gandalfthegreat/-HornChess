{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module View where

import Board
import qualified Board as Chess
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

-------------------------------------------------------Helper Function----------------------------------------------------

-- get black and white Chess position
---------------------------------------------------------constant------------------------------------------------------------------
type Name = ()

cw :: Widget Name
cw = str " " -- pixel is 2 spaces

height = 58

width = 64

--------------------------------------------------draw function-----------------------------------------------------------------------------------------------------
draw :: PlayState -> [Widget Name]
draw st = [C.center (drawGrid st)]

drawGrid :: PlayState -> Widget Name
drawGrid st =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "HornChess" <+> str "white in" <+> str (show (findpos White (psBoard st)))) $
      vBox rows
  where
    -- draw all the cells in the grid.
    -- a cell represent  disk, peg or Empty.
    rows = [hBox $ cellsInRow r | r <- [height -1, height -2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width -1]]
    drawCoord = drawCell . cellAt
    b = psBoard st
    cellAt c -- give a coordinate, return a state of pixel
      | isPartOfWhite c b = WhiteChess
      | isPartOfBlack c b = BlackChess
      | isPartOfBoard c = Board
      | otherwise = Background

-- draw a cell
drawCell :: Pixel -> Widget Name
drawCell Board = withAttr boardAttr cw
drawCell Background = withAttr bgAttr cw
drawCell WhiteChess = withAttr whiteAttr cw
drawCell BlackChess = withAttr blackAttr cw

--------------------------------------------------get cell to render -------------------------------------

indexToBase :: [(Integer, Integer)]
indexToBase = [(24, 89), (23, 79)]

isPartOfBoard :: V2 Integer -> Bool
isPartOfBoard c = c `elem` pegBase

isPartOfBlack c b = c `elem` blackList b

isPartOfWhite c b = c `elem` (whiteList b)

-- whiteList = [V2 01 02]

blackList :: M.Map Int Chess -> [V2 Integer]
blackList b = chessRenderBlack b

whiteList :: M.Map Int Chess -> [V2 Integer]
whiteList b = chessRenderWhite b

-- [ V2 40 1,
--   V2 4 1,
--   V2 41 12,
--   V2 8 12,
--   V2 43 23,
--   V2 12 23,
--   V2 44 34,
--   V2 16 34,
--   V2 47 42,
--   V2 25 42,
--   V2 51 48,
--   V2 34 48,
--   V2 56 54
-- ]

--------------------------------------------------hard code chess-------------------------------------
pegBase :: [V2 Integer]
pegBase = horizontalLine ++ zone1 ++ zone2 ++ zone3 ++ zone4 ++ zone5 ++ zone6

basePos =
  -- base position for chess
  [ V2 40 1,
    V2 4 1,
    V2 41 12,
    V2 8 12,
    V2 43 23,
    V2 12 23,
    V2 44 34,
    V2 16 34,
    V2 47 42,
    V2 25 42,
    V2 51 48,
    V2 34 48,
    V2 56 54
  ]

--  >>> initialBoard
-- chessShape

-- >>> chessRow
chessRow y = fmap (\x -> V2 x y) [0 .. 3] -- generate one row of the chess

chessRows = concat (fmap (\y -> chessRow y) [0, 1]) -- generate whole chess

-- initialBoard
chessRenderBlack b = concat (fmap (\x -> checkBlack x b) [0 .. 12])

checkBlack x b =
  case b M.! x of
    Black1 -> fmap (\y -> (basePos !! x) + y) chessRows
    Black2 -> fmap (\y -> (basePos !! x) + y) chessRows
    otherwise -> []

chessRenderWhite b = concat (fmap (\x -> checkWhite x b) [0 .. 12])

checkWhite :: Int -> M.Map Int Chess -> [V2 Integer]
checkWhite x b =
  case b M.! x of
    White -> fmap (\y -> (basePos !! x) + y) chessRows
    otherwise -> []

-- boardShape
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
  fmap (\x -> V2 x 2) ([5] ++ [38 .. 40] ++ [42])
    ++ fmap (\x -> V2 x 3) ([5] ++ [35 .. 37] ++ [42])
    ++ fmap (\x -> V2 x 4) ([6] ++ [32 .. 34] ++ [42])
    ++ fmap (\x -> V2 x 5) ([6] ++ [29 .. 31] ++ [42])
    ++ fmap (\x -> V2 x 6) ([7] ++ [26 .. 28] ++ [42])
    ++ fmap (\x -> V2 x 7) ([7] ++ [23 .. 25] ++ [43])
    ++ fmap (\x -> V2 x 8) ([7] ++ [20 .. 22] ++ [43])
    ++ fmap (\x -> V2 x 9) ([8] ++ [17 .. 19] ++ [43])
    ++ fmap (\x -> V2 x 10) ([8] ++ [14 .. 16] ++ [43])
    ++ fmap (\x -> V2 x 11) ([9] ++ [11 .. 13] ++ [43])
    ++ fmap (\x -> V2 x 12) ([9] ++ [10] ++ [43])

zone2 =
  fmap (\x -> V2 x 14) ([9, 40, 41, 43])
    ++ fmap (\x -> V2 x 15) ([9] ++ [37 .. 39] ++ [43])
    ++ fmap (\x -> V2 x 16) ([10, 35, 36, 43])
    ++ fmap (\x -> V2 x 17) ([10] ++ [32 .. 34] ++ [44])
    ++ fmap (\x -> V2 x 18) ([10, 30, 31, 44])
    ++ fmap (\x -> V2 x 19) ([11] ++ [27 .. 29] ++ [44])
    ++ fmap (\x -> V2 x 20) ([11, 25, 26, 44])
    ++ fmap (\x -> V2 x 21) ([12] ++ [22 .. 24] ++ [44])
    ++ fmap (\x -> V2 x 22) ([12, 20, 21, 44])
    ++ fmap (\x -> V2 x 23) ([13] ++ [17 .. 19] ++ [45])
    ++ fmap (\x -> V2 x 24) ([13, 15, 16, 45])

-- ....................................................
zone3 =
  fmap (\x -> V2 x 26) ([13] ++ [41 .. 43] ++ [45])
    ++ fmap (\x -> V2 x 27) ([14] ++ [39 .. 40] ++ [45])
    ++ fmap (\x -> V2 x 28) ([14] ++ [36 .. 38] ++ [45])
    ++ fmap (\x -> V2 x 29) ([15] ++ [33 .. 35] ++ [45])
    ++ fmap (\x -> V2 x 30) ([15] ++ [31, 32] ++ [45])
    ++ fmap (\x -> V2 x 31) ([16] ++ [28 .. 30] ++ [46])
    ++ fmap (\x -> V2 x 32) ([16] ++ [25 .. 27] ++ [46])
    ++ fmap (\x -> V2 x 33) ([17] ++ [23, 24] ++ [46])
    ++ fmap (\x -> V2 x 34) ([17] ++ [20 .. 22] ++ [46])
    ++ fmap (\x -> V2 x 35) ([17] ++ [33 .. 35] ++ [45])

zone4 =
  fmap (\x -> V2 x 36) ([18] ++ [45] ++ [46])
    ++ fmap (\x -> V2 x 37) ([19, 20] ++ [42 .. 44] ++ [46])
    ++ fmap (\x -> V2 x 38) ([20, 21] ++ [38 .. 41] ++ [47])
    ++ fmap (\x -> V2 x 39) ([22] ++ [35 .. 37] ++ [47])
    ++ fmap (\x -> V2 x 40) ([23] ++ [32 .. 34] ++ [48])
    ++ fmap (\x -> V2 x 41) ([24, 25] ++ [28 .. 31] ++ [48])
    ++ fmap (\x -> V2 x 42) ([26] ++ [27] ++ [49])

zone5 =
  fmap (\x -> V2 x 44) ([27, 28] ++ [48] ++ [49])
    ++ fmap (\x -> V2 x 45) ([29] ++ [45 .. 47] ++ [50])
    ++ fmap (\x -> V2 x 46) ([30, 31] ++ [43, 44] ++ [50])
    ++ fmap (\x -> V2 x 47) ([32, 33] ++ [40 .. 42] ++ [51])
    ++ fmap (\x -> V2 x 48) ([34] ++ [37 .. 39] ++ [52])

zone6 =
  fmap (\x -> V2 x 50) ([37, 38] ++ [53])
    ++ fmap (\x -> V2 x 51) ([39 .. 41] ++ [54])
    ++ fmap (\x -> V2 x 52) ([42 .. 44] ++ [55])
    ++ fmap (\x -> V2 x 53) ([45, 46] ++ [56])
    ++ fmap (\x -> V2 x 54) ([47 .. 49] ++ [56])
    ++ fmap (\x -> V2 x 55) ([50 .. 52] ++ [57])

--   fmap (\x -> V2 x 50) ([37, 38] ++ [53])
--     ++ fmap (\x -> V2 x 51) ([39 .. 41] ++ [54])
--     ++ fmap (\x -> V2 x 52) ([42 .. 44] ++ [55])
--     ++ fmap (\x -> V2 x 53) ([45, 46] ++ [56])
--     ++ fmap (\x -> V2 x 54) ([47 .. 49] ++ [56])
--     ++ fmap (\x -> V2 x 55) ([50 .. 52] ++ [57])

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
    [ (bgAttr, grey `on` grey), -- Background
      (blackAttr, V.yellow `on` V.yellow), -- Black Chess
      (whiteAttr, V.white `on` V.white), -- White Chess
      (boardAttr, darkBlack `on` darkBlack) -- Board, outline of HornChess
    ]
