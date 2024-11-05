module Helper (saveOrPrint) where

import Data.List (unwords)

saveOrPrint :: [[Int]] -> [String] -> IO ()
saveOrPrint sudokuBoard [filepath] = saveBoard sudokuBoard filepath 
saveOrPrint sudokuBoard []         = printBoard sudokuBoard
saveOrPrint _ _                    = putStrLn "Option Invalid"

saveBoard :: [[Int]] -> String -> IO ()
saveBoard sudokuBoard filepath = 
    (writeFile filepath) ((unlines . map unwords . map (map show)) sudokuBoard)

printBoard :: [[Int]] -> IO ()
printBoard [] = putStrLn "---"
printBoard (line:lineS) = do
    let lineString = map show line
    putStrLn $ unwords lineString
    printBoard lineS