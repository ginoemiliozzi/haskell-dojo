module TTT.A1 where

import Data.Char (toUpper)

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02

_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex idx = (fromEnum (toUpper idx)) - 65

-- Q#04

_INVALID_MOVE_ = (-1, -1)

-- Q#05

_SEP_ = ['_', '|', '_']

-- Q#06

data Square = X | O | EmptySquare deriving (Show, Eq)

-- Q#07

data GameState = X_Won | O_Won | Tie | InProgress deriving (Show, Eq)

-- Q#08
type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)

-- Q#09

getFirstPlayer xFirst = if xFirst then X else O

getFirstPlayer_ :: Bool -> Square
getFirstPlayer_ xFirst
   | xFirst = X
   | not xFirst = O 

-- Q#10
showGameState :: GameState -> String
showGameState gameState = case gameState of 
    X_Won      -> "Player X is the winner"
    O_Won      -> "Player O is the winner"
    Tie        -> "It was a tie"
    InProgress -> "The game is still in progress"
       

-- Q#11

switchPlayer X = O
switchPlayer O = X
switchPlayer _ = EmptySquare

-- Q#12

showSquare sq = case sq of
    X -> "X"
    O -> "O"
    EmptySquare -> "_"