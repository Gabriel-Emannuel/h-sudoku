module GenerateFuncionality (generate) where

import Resolver (resolveSudoku)

import System.Random

import Control.Monad (replicateM)

import Helper (saveOrPrint)

import Data.List (delete)

generate :: [String] -> IO ()
generate [] = showCommands
generate (dimension: args) = do
    firstLine <- generateLineRandom [1..lineAmmount]
    let sudokuBoard = firstLine : linesNotRandom
    let sudokuSolved = resolveSudoku sudokuBoard
    indexesForTake <- replicateM lineAmmount (generateLineRandom [0..pred lineAmmount])
    muchTakeForLine <- replicateM lineAmmount (randomRIO (0, lineAmmount) :: IO Int)
    not0 <- randomRIO (not0', not0'') :: IO Int
    let muchTakeForLine' = treatRandom not0 muchTakeForLine
    let indexesFortake' = zipWith take muchTakeForLine' indexesForTake
    let sudokuForResolve = zipWith (takeNumbers 0) indexesFortake' sudokuSolved 
    saveOrPrint sudokuForResolve args
    where
        d = convertInt dimension
        lineAmmount = d ^ 2
        linesNotRandom = [[0 | _ <- [1..lineAmmount]] | _ <- [1..pred lineAmmount]]
        not0'  = pred (2 * lineAmmount) 
        not0'' = (lineAmmount ^ 2) `div` 2

generate _ = putStrLn "Put hsudoku generate for see the commands"

treatRandom :: Int -> [Int] -> [Int]
treatRandom n totalForTake
    | sum nextTotalForTake < n = totalForTake
    | otherwise = treatRandom n nextTotalForTake
    where
        nextTotalForTake = map pred totalForTake

takeNumbers :: Int -> [Int] -> [Int] -> [Int]
takeNumbers _ [] numbers = numbers
takeNumbers _ _ [] = []
takeNumbers i indexes (number: numbers)
    | i `elem` indexes = 0 : takeNumbers (succ i) indexes numbers
    | otherwise = number : takeNumbers (succ i) indexes numbers

generateLineRandom :: [Int] -> IO [Int]
generateLineRandom []   = return []
generateLineRandom line = do
    index <- randomRIO (0, pred (length line)) :: IO Int
    let number = line !! index
    let lineWithoutNumber = delete number line
    newLineRandom <- generateLineRandom lineWithoutNumber
    return $ number : newLineRandom

showCommands :: IO ()
showCommands = do
    putStrLn "hsudoku generate [dimension] [optional: dest-file-path]"
    putStrLn "  dimension: a positive number bigger than 0, this is the dimension for a quadrant in the board!"
    putStrLn "             if you put a value of 3, the board gonna have quadrants with the size of 3, this means the game is 9x9!"
    putStrLn "  dest-file-path: if presented in the input, the sudoku isn't printed in the terminal but saved in this file"

convertInt :: String -> Int
convertInt v = read v :: Int