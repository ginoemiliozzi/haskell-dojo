module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01

showInts (i: is) = show i : showInts is
showInts [] = []

_HEADER_ = ' ' : formatLine (showInts _RANGE_)

-- Q#02

showSquares (sq: sqs) = showSquare sq : showSquares sqs
showSquares [] = []

-- Q#03
formatRows :: [Row] -> [String]
formatRows (r: rs) = formatLine (showSquares r) : formatRows rs
formatRows [] = []

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty row idx = go row idx 0 where
    go :: Row -> Int -> Int -> Bool
    go (sq : sqs) idx currIdx = idx == currIdx && isEmpty sq || go sqs idx (currIdx + 1)
    go [] _ _ = False

    isEmpty :: Square -> Bool
    isEmpty sq = sq == EmptySquare


-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol (r : rs) = tail r : dropFirstCol rs
dropFirstCol [] = []

dropLastCol :: Board -> Board
dropLastCol (r : rs) = init r : dropLastCol rs
dropLastCol [] = []

-- Q#06

getDiag1 :: Board -> Line
getDiag1 (r : rs) = head r : getDiag1 (dropFirstCol rs)
getDiag1  [] = []    

getDiag2 :: Board -> Line
getDiag2 (r : rs) = last r : getDiag2 (dropLastCol rs)
getDiag2  [] = []    

getAllLines :: Board -> [Line]
getAllLines board = board ++ transpose board ++ [getDiag1 board, getDiag2 board]

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare p b m = go p b m 0 where
    go :: Player -> Board -> Move -> Int -> Board
    go player (r : rs) (mr, mc) idxR = if idxR == mr then replaceSquareInRow player mc r : rs else r : go player rs (mr, mc) (idxR + 1)
    go _ [] _ _ = []


-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices strs = go [] (zip strs ['A' .. 'Z']) where
    go :: [String] -> [(String, Char)] -> [String]
    go acc ((s, c): scs) = go ((c : s) : acc) scs 
    go acc _  = reverse acc

-- Q#09

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ player l = go l False where
    go :: Line -> Bool -> Bool
    go (sq : sqs) acc = (player == sq) && go sqs True
    go [] acc = acc

-- Q#10
isValidMove :: Board -> Move -> Bool
isValidMove board move = if isMoveInBounds move then go 0 board move else False where
    go :: Int -> Board -> Move -> Bool
    go idx (r : rs) (rowN, colN) = if idx == rowN then isColEmpty r colN else go (idx + 1) rs (rowN, colN)
    go _ [] _ = False