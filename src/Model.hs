module Model where

import Board
-- declaration of State, we put all data structures in St

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { 
    psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.Chess       -- ^ whose turn 
  , psResult :: Board.Result Board.Board -- ^ result      
  } 

initialState :: PlayState
initialState = PS 
  { 
    psBoard  = Board.initialBoard
  , psTurn   = Board.White
  , psResult = Board.Cont Board.initialBoard
  }


next :: PlayState -> Board.Result Board.Board -> Either (Board.Result Board.Board) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psBoard = b'
                                  , psTurn  = Board.flipXO (psTurn s) })
next s res             = Left res
                    
