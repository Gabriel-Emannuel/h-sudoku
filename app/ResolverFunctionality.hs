module ResolverFunctionality (resolve) where

import Util (isValidSudoku)

import Helper (saveOrPrint)

import Resolver (resolveSudoku)

import Data.List.Split (splitOn)

resolve :: [String] -> IO ()
resolve [] = showCommands
resolve (filepath: args) = do
    sudokuBoard <- readSudoku filepath
    if isValidSudoku sudokuBoard
    then do 
        let sudokuResolved = resolveSudoku sudokuBoard
        if null sudokuResolved 
        then putStrLn "Sudoku Tried, But not Valid"
        else saveOrPrint sudokuResolved args 
        return ()
    else putStrLn "This sudoku isn't valid for the try"
resolve _ = errorMessage

showCommands :: IO ()
showCommands = do
    putStrLn "hsudoku resolve [file-path] [optional: dest-file-path]"
    putStrLn "  file-path:      The CSV File what represents the sudoku board"
    putStrLn "  dest-file-path: If is presented, the resolved sudoku isn't showed but is saved in this path"

errorMessage :: IO ()
errorMessage = putStrLn "Please, put \"hsudoku resolve\" for see the commands"

readSudoku :: String -> IO [[Int]]
readSudoku filepath = do
    txtContent <- readFile filepath
    let linesContent = lines txtContent
    let sudokuBoard = map (splitOn ",") linesContent
    return $ map (map convertInt) sudokuBoard

convertInt :: String -> Int
convertInt v = read v :: Int