module Main where

import Fundamentals (tests)

import Boards (resolvingBoards)

import Test.HUnit (runTestTT)


main :: IO()
main = do
  runTestTT tests
  runTestTT resolvingBoards
  return ()