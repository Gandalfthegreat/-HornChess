import Board
import Data.Map as M
import System.Exit


testBoard1 :: Board.Board 
testBoard1 = M.fromList (zip [0 .. 12] [Black1, Black2, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, White])

testBoardDirMove :: Board.Board               --0       1      2      3      4      5
testBoardDirMove = M.fromList (zip [0 .. 12] [Empty, Empty, Black1, Empty, Empty, Black2, Empty, Empty, Empty, Empty, Empty, Empty, White])

testBoard2 :: Board
testBoard2 = M.fromList (zip [0..12] [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Black1,Black2,White])

testBoard3 :: Board
testBoard3 = M.fromList (zip [0..12] [White,Black2,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Black1])

testFindpos :: Chess -> Board -> Int -> Bool
testFindpos c b p = p == findpos c b

unittestpos :: Bool
unittestpos = (testFindpos White testBoard1 12) && (testFindpos Black1 testBoard1 0) && (testFindpos Black2 testBoard1 1)

testPutB :: Board -> Board -> Int -> Int -> Chess -> Bool
testPutB b1 b2 p1 p2 c = case putB b1 c p1 p2 of
    Cont b -> b==b2
    otherwise -> False

unittestput :: Bool
unittestput = (testPutB testBoard1 (M.fromList (zip [0..12] [Empty,Black2,Black1,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,White])) 0 2 Black1) 
    && (testPutB testBoard1 (M.fromList (zip [0..12] [Black1,Empty,Empty,Black2,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,White])) 1 3 Black2)
    && (testPutB testBoard1 (M.fromList (zip [0..12] [Black1,Black2,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,White,Empty])) 12 11 White)

testWinBlack :: Board -> Bool -> Bool
testWinBlack b r = (winsblack b) == r

testWinWhite :: Board -> Bool -> Bool
testWinWhite b r = (winswhite b) == r

unittestwin :: Bool
unittestwin = (testWinBlack testBoard1 False) && (testWinWhite testBoard1 False) && (testWinBlack testBoard2 True) && (testWinWhite testBoard3 True)

unittestDirMove :: String -> Bool
unittestDirMove s = case s of
    "up" -> (up 2 testBoardDirMove == 4) && (up 1 testBoardDirMove == 1)
    "right" -> (right 5 testBoardDirMove == 4) && (right 1 testBoard1 == 1)
    "down" -> (down 2 testBoardDirMove == 0) && (down 0 testBoard1 == 0)
    "left" -> (left 2 testBoardDirMove == 3) && (left 0 testBoard1 == 0)
    "upleft" -> (upLeft 6 testBoardDirMove == 9) && (upLeft 3 testBoardDirMove == 3)
    "upright" -> (upRight 1 testBoardDirMove == 3) && (upRight 0 testBoardDirMove == 0)
    "downleft" -> (downLeft 5 testBoardDirMove == 3) && (downLeft 0 testBoardDirMove == 0)
    "downright" -> (downRight 9 testBoardDirMove == 6) && (downRight 0 testBoardDirMove == 0)
    _ -> False 

main :: IO ()
main = do
    if unittestpos
    then
        putStrLn "Passed move position tests"
    else
        putStrLn "Failed move position tests"
    if unittestpos
    then
        putStrLn "Passed find position tests"
    else
        putStrLn "Failed find position tests"
    if unittestwin
    then
        putStrLn "Passed win judge tests"
    else
        putStrLn "Failed win judge tests"
    if unittestDirMove "up" && unittestDirMove "right" && unittestDirMove "down" && unittestDirMove "left" && unittestDirMove "upleft" && unittestDirMove "upright" && unittestDirMove "downleft" && unittestDirMove "downright"
    then 
        putStrLn "Passed Direction Move Tests"
    else
        putStrLn "Failed Direction Move Tests"
-- >>> testDirMove "downright"
-- True
-- 
--

