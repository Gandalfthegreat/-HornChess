module Control where

import Board
import Brick hiding (Result)
import Brick.Main
import Brick.Types
import qualified Brick.Types as T
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map as M
import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events
import Model

-- import Model.Player

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n e -> EventM n (Next PlayState)
control s ev = case ev of
  T.VtyEvent (V.EvKey (V.KChar 's') []) -> Brick.continue (flipblack s)
  T.VtyEvent (V.EvKey (V.KChar 'w') []) -> nextS s =<< liftIO (play1 s up)
  T.VtyEvent (V.EvKey (V.KChar 'e') []) -> nextS s =<< liftIO (play1 s upRight)
  T.VtyEvent (V.EvKey (V.KChar 'd') []) -> nextS s =<< liftIO (play1 s right)
  T.VtyEvent (V.EvKey (V.KChar 'c') []) -> nextS s =<< liftIO (play1 s downRight)
  T.VtyEvent (V.EvKey (V.KChar 'x') []) -> nextS s =<< liftIO (play1 s down)
  T.VtyEvent (V.EvKey (V.KChar 'z') []) -> nextS s =<< liftIO (play1 s downLeft)
  T.VtyEvent (V.EvKey (V.KChar 'a') []) -> nextS s =<< liftIO (play1 s left)
  T.VtyEvent (V.EvKey (V.KChar 'q') []) -> nextS s =<< liftIO (play1 s upLeft)
  T.VtyEvent (V.EvKey V.KEsc _) -> Brick.halt s
  _ -> Brick.continue s -- Brick.halt s

----test
-- let f x = if x == 1 then Just White else Nothing
-- nextBoard =
--   M.fromList (zip [0 .. 12] [Empty, Black2, Empty, Empty, Empty, Black1, Empty, Empty, Empty, Empty, Empty, Empty, White])

-- control :: PlayState -> BrickEvent n1 e -> EventM n2 (Next PlayState)
-- control s ev = case ev of
--   T.VtyEvent (V.EvKey (V.KChar 's') []) -> Brick.continue (flipblack s)
--   T.VtyEvent (V.EvKey (V.KChar 'r') []) -> continue s {psBoard = nextBoard, psTurn = Black1}

flipblack :: PlayState -> PlayState
flipblack s = case b of
  Black1 -> s {psTurn = Black2}
  Black2 -> s {psTurn = Black1}
  _ -> s
  where
    b = psTurn s

play1 :: PlayState -> (Int -> Board -> Int) -> IO (Board.Result Board)
play1 s f
  | (pos==12 || (pos-npos)>1) && (xo==Black2 || xo == Black1) = return Retry
  | pos == npos = return Retry
  | otherwise = return $ putB bo xo pos npos
  where
    xo = psTurn s
    bo = psBoard s
    pos = Board.findpos xo bo
    npos = f pos bo

-------------------------------------------------------------------------------
nextS :: PlayState -> Board.Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s b of
  Right s' -> continue s'
  Left res -> halt (s {psResult = res})