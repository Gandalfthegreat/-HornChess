module Model where

-- declaration of State, we put all data structures in St
data St = St
  { board :: String
  }

initialState =
  St
    { board = "X X O"
    }