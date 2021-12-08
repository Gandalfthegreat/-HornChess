{-# LANGUAGE DeriveFunctor #-}
module Board 
  where

import qualified Data.Map as M 
import Data.Set as Set ( fromList, notMember, Set )

type Board = M.Map Int Chess

data Chess
    = Black1
    | Black2
    | White
    | Empty 
    deriving (Eq, Show)

data Result a 
  = Win Chess
  | Retry 
  | Cont a
  deriving (Eq, Functor, Show)

putB :: Board -> Chess -> Int -> Int -> Result Board
putB b x p np = result $ M.insert np x $ M.insert p Empty b

result :: Board -> Result Board
result b 
  | winsblack b    = Win Black1
  | winswhite b    = Win White
  | otherwise = Cont b


(!-) :: Board -> Int -> Chess 
board !- pos = case M.lookup pos board of
  Just b -> b
  Nothing -> Empty

winsblack :: Board -> Bool
winsblack b = 
    (c1 == Black1 || c1 == Black2) && (c2 == Black1 || c2 == Black2) && c3 == White
    where
      c1 = b !- 10
      c2 = b !- 11
      c3 = b !- 12

winswhite :: Board -> Bool
winswhite b = 
    c1 == White || c2 == White
    where
      c1 = b !- 0
      c2 = b !- 1

flipXO :: Chess -> Chess
flipXO White = Black1
flipXO _     = White

dirMap :: [[Int]]
dirMap = [
    [2, -1, -1, -1, -1, -1, 1, 3], --0
    [-1, 3, 0, -1, -1, -1, -1, -1], --1
    [4, -1, -1, -1, 0, -1, 3, 5], --2
    [-1, 5, 2, 0, -1, 1, -1, -1], --3
    [6, -1, -1, -1, 2, -1, 5, 7], --4
    [-1, 7, 4, 2, -1, 3, -1, -1], -- 5
    [8, -1, -1, -1, 4, -1, 7, 9], --6
    [-1, 9, 6, 4, -1, 5, -1, -1], --7
    [10, -1, -1, -1, 6, -1, 9, 11],--8
    [-1, 11, 8, 6, -1, 7, -1, -1], --9
    [12, -1, -1, -1, 8, -1, 11, -1], --10
    [-1, 12, 10, 8, -1, 9, -1, -1], --11
    [-1, -1, -1, -1, 10, 11, -1, -1]] --12


adjacencyMatrix::[[Int]]
adjacencyMatrix = [[1, 0, 0, 0, 0, 0, 1, 1], --0
               [0, 1, 1, 0, 0, 0, 0, 0], --1
               [1, 0, 0, 0, 1, 0, 1, 1], --2
               [0, 1, 1, 1, 0, 1, 0, 0], --3
               [1, 0, 0, 0, 1, 0, 1, 1], --4
               [0, 1, 1, 1, 0, 1, 0, 0], --5
               [1, 0, 0, 0, 1, 0, 1, 1], --6
               [0, 1, 1, 1, 0, 1, 0, 0], --7
               [1, 0, 0, 0, 1, 0, 1, 1], --8
               [0, 1, 1, 1, 0, 1, 0, 0], --9
               [1, 0, 0, 0, 1, 0, 1, 1], --10
               [0, 1, 1, 1, 0, 1, 0, 0], --11
               [0, 0, 0, 0, 1, 1, 0, 0]] -- 12


-- 状态初始化
initialBoard :: Board
initialBoard = 
    M.fromList (zip [0..12] [Black1,Black2,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,White])
-- 移动

findpos :: Chess -> Board -> Int
findpos chs bb = findapos chs $ M.toList bb

findapos :: Chess -> [(Int,Chess)] -> Int
findapos chs [] = 0
findapos chs bb@(x:xs)
  | chs == snd x = fst x
  | otherwise = findapos chs xs

-- 判断win
-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

up :: Int -> Board -> Int
up p board = if head (adjacencyMatrix !! p) == 1
    then 
    case M.lookup p board of
        Just Empty  -> head (dirMap !! p)
        _ -> p
    else p


down :: Int -> Board -> Int
down p board = if (adjacencyMatrix !! p) !! 4 == 1
    then 
    case M.lookup p board of
        Just Empty  -> (dirMap !! p) !!4
        _ -> p
    else p


left :: Int -> Board -> Int
left p board = if (adjacencyMatrix !! p) !! 6 == 1
    then 
    case M.lookup p board of
        Just Empty  -> (dirMap !! p) !!6
        _ -> p
    else p


right :: Int -> Board -> Int
right p board = if (adjacencyMatrix !! p) !! 2 == 1
    then 
    case M.lookup p board of
        Just Empty  -> (dirMap !! p) !! 2
        _ -> p
    else p


upRight :: Int -> Board -> Int
upRight p board = if (adjacencyMatrix !! p) !! 1 == 1
    then 
    case M.lookup p board of
        Just Empty  -> (dirMap !! p) !! 1
        _ -> p
    else p


upLeft :: Int -> Board -> Int
upLeft p board = if (adjacencyMatrix !! p) !! 7 == 1
    then 
    case M.lookup p board of
        Just Empty  -> (dirMap !! p) !! 7
        _ -> p
    else p


downRight :: Int -> Board -> Int
downRight p board = if (adjacencyMatrix !! p) !! 3 == 1
    then 
    case M.lookup p board of
        Just Empty  -> (dirMap !! p) !! 3
        _ -> p
    else p

downLeft :: Int -> Board -> Int
downLeft p board = if (adjacencyMatrix !! p) !! 5 == 1
    then 
    case M.lookup p board of
        Just Empty  -> (dirMap !! p) !! 5
        _ -> p
    else p