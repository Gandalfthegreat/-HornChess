module Model where

import Board

-- declaration of State, we put all data structures in St

data State
  = Intro
  | Play PlayState
  | Outro

data PlayState = PS
  { -- | current board
    psBoard :: Board.Board,
    -- | whose turn
    psTurn :: Board.Chess,
    -- | result
    psResult :: Board.Result Board.Board
  }

initialState :: PlayState
initialState =
  PS
    { psBoard = Board.initialBoard,
      psTurn = Board.White,
      psResult = Board.Cont Board.initialBoard
    }

next :: PlayState -> Board.Result Board.Board -> Either (Board.Result Board.Board) PlayState
next s Board.Retry = Right s
next s (Board.Cont b') =
  Right
    ( s
        { psBoard = b',
          psTurn = Board.flipXO (psTurn s)
        }
    )
next s res = Left res

data Pixel
  = BlackChess -- pixel for black chess (virtually Yellow)
  | WhiteChess -- pixel for white chess (virtually White)
  | Background -- pixel for bg (virtually grey)
  | Board -- pixel for board (virtually black)
  deriving (Eq, Show)