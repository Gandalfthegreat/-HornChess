module Model where

-- declaration of State, we put all data structures in St
data St = St
  { board :: String
  }

data Chess
  = Black1
  | Black2
  | White
  | Empty -- board in black
  deriving (Eq, Show)

data Pixel
  = BlackChess -- pixel for black chess (virtually Yellow)
  | WhiteChess -- pixel for white chess (virtually White)
  | Background -- pixel for bg (virtually grey)
  | Board -- pixel for board (virtually black)
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