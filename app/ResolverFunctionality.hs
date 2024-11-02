module ResolverFunctionality (resolve) where

import Util (isValidSudoku)

import Resolver (resolveSudoku)

import Data.List(unwords)

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

saveOrPrint :: [[Int]] -> [String] -> IO ()
saveOrPrint _ [_] = putStrLn "Saving CSV File not implemented Yet"
saveOrPrint sudokuBoard []         = printBoard sudokuBoard
saveOrPrint _ _                    = putStrLn "Option Invalid"

printBoard :: [[Int]] -> IO ()
printBoard [] = putStrLn "---"
printBoard (line:lineS) = do
    let lineString = map show line
    putStrLn $ unwords lineString
    printBoard lineS

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