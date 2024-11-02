module Main where

import System.Environment (getArgs)

import ResolverFunctionality (resolve)

import GenerateFuncionality (generate)

main :: IO ()
main = do
    args <- getArgs
    selectFunctionality args

selectFunctionality :: [String] -> IO ()
selectFunctionality ["hsudoku"]                     = showCommands
selectFunctionality ("hsudoku" : "resolve"  : args) = resolve args
selectFunctionality ("hsudoku" : "generate" : args) = generate args
selectFunctionality _                               = errorMessage

showCommands :: IO ()
showCommands = do
    putStrLn "hsudoku [funcionality]"
    putStrLn "resolve:"
    putStrLn "  From a csv file representing a sudoku board, gonna be printed the resolved sudoku"
    putStrLn "generate:"
    putStrLn "  TODO"

errorMessage :: IO ()
errorMessage = putStrLn "Input \"hsudoku\" for see the possible commands" 