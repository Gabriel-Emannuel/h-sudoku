module Main where

import Lib (writeProtocol, readProtocol)

import Helpers.HelpMessage (printHelp)
import Helpers.ErrorArgumentsMessage (printError)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of 
        ("help":_) -> printHelp
        ("read": filePath: otherArguments) -> readProtocol filePath otherArguments
        ("create": otherArguments) -> writeProtocol otherArguments
        _ -> printError