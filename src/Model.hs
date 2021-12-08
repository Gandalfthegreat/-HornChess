module Model where

-- declaration of State, we put all data structures in St
data St = St
  { board :: String
  }

data Chess
  = Black1
  | Black2
  | White
  | Empty
  deriving (Eq, Show)

initialState =
  St
    { board = "X X O"
    }

--键盘操作
--状态变化
--判断赢输
--绘图
--