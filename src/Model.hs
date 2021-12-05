module Model where

data St = St
  { board :: String
  }

initialState =
  St
    { board = "X X O"
    }