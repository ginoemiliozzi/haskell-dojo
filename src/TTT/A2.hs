module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01

promptPlayer sq = concat [
    "Player ",
    show sq,
    "'s turn: enter a row and column position (ex. A1)"
    ]

-- Q#02

_RANGE_ = [0 .. (_SIZE_ - 1)]

-- Q#03
isDigit d = d `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit d
   | isDigit d = read [d]
   | otherwise = -1


-- Q#04
_EMPTY_ROW_ = replicate _SIZE_ EmptySquare

_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied :: Board -> Bool
isTied board = EmptySquare `notElem` concat board

_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06

indexRowStrings strs = ['A' ..] `zip` strs

-- Q#07
formatLine strs = _SEP_ ++ intercalate _SEP_ strs ++ _SEP_

-- Q#08

isMoveInBounds (row, col) = and inBounds
    where inBounds = [
            row >= 0,
            row < _SIZE_,
            col >= 0,
            col < _SIZE_
            ]

-- Q#09

stringToMove [row, col] = (rowN, colN)
    where rowN = convertRowIndex row
          colN = readDigit col

stringToMove _ = _INVALID_MOVE_

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player idx row = if isValid then replaceIt else row
    where isValid = idx >= 0 && idx < length row
          (before, rest) = splitAt idx row
          replaceIt = take (length row) (before ++ [player] ++ tail rest)