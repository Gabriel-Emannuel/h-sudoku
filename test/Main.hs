module Main where

import Test.HUnit

import ResolverTests.Base(baseTests4x4, baseTests9x9)

main :: IO()
main = do
  runTestTT baseTests4x4
  runTestTT baseTests9x9
  return ()