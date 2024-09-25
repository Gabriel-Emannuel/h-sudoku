module ResolverTests.Business (busisnessTest) where

import ResolverTests.Consts (board9x9, board4x4)

import Resolver(witchQuadrant, insert, updateNumsMiss)

import Test.HUnit (Test(..), assertEqual, assertBool)

--- witch quadrant

test1 :: Test
test1 = TestCase (assertEqual "Testing quadrant = 0" 0 (witchQuadrant 3 (0,0)))

test2 :: Test
test2 = TestCase (assertEqual "Testing quadrant = 1" 1 (witchQuadrant 3 (4,0)))

test3 :: Test
test3 = TestCase (assertEqual "Testing quadrant = 4" 5 (witchQuadrant 3 (8,5)))

test4 :: Test
test4 = TestCase (assertEqual "Testing quadrant = 8" 8 (witchQuadrant 3 (8,8)))

test5 :: Test
test5 = TestCase (assertEqual "Testing quadrant = 0" 0 (witchQuadrant 2 (0,0)))

test6 :: Test
test6 = TestCase (assertEqual "Testing quadrant = 1" 1 (witchQuadrant 2 (2,0)))

test7 :: Test
test7 = TestCase (assertEqual "Testing quadrant = 2" 2 (witchQuadrant 2 (0,3)))

test8 :: Test
test8 = TestCase (assertEqual "Testing quadrant = 3" 3 (witchQuadrant 2 (3,3)))

quadrantTests :: Test
quadrantTests = TestList [test1, test2, test3, test4, test5, test6, test7, test8]

---

--- insert

test9 :: Test
test9 = TestCase (assertEqual "Testing insertion board 4x4" ([[1,2,3,0], [0,4,0,2], [2,0,4,1],[0,1,0,0]]) (insert (1,0) 2 board4x4))

insertTests :: Test
insertTests = TestList [test9]

---

--- update num miss

test10 :: Test
test10 = TestCase (assertEqual "Testing update num miss" [[1,5],[2]] (updateNumsMiss 0 3 [[1,3,5], [2]]))

updateTestMiss :: Test
updateTestMiss = TestList [test10]

---

busisnessTest :: Test
busisnessTest = TestList [quadrantTests, insertTests, updateTestMiss]