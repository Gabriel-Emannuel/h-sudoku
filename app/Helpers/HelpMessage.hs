module Helpers.HelpMessage (printHelp) where

printHelp :: IO ()
printHelp = do
    putStrLn "Usage: h-sudoku help"
    putStrLn "       h-sudoku [-r | read] [file path] [optional: -s file path]"
    putStrLn "       h-sudoku [-c | create] [optional: -d game dimesional] [optional: --eazy | --hard ] [optional: -s file path]"
    putStrLn "For more informations, see the README file."