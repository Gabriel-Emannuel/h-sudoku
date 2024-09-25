module Main where

import ResolverTests.Base(baseTests4x4, baseTests9x9)
import ResolverTests.Business(busisnessTest)
import ResolverTests.Main(mainTests)
import Test.HUnit (runTestTT)


main :: IO()
main = do
  runTestTT baseTests4x4
  runTestTT baseTests9x9
  runTestTT busisnessTest
  runTestTT mainTests
  return ()