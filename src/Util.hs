module Util where

import Resolver (resolveSudoku)

isValidSudoku :: [[Int]] -> Bool
isValidSudoku sudoku =
    all (\x -> length x == length sudoku) sudoku
    && not (any (any (`notElem` domain)) sudoku)
    where
        domain = [0..length sudoku]