module Boards (resolvingBoards) where

import Test.HUnit (Test(TestCase, TestList), assertEqual)

import Resolver (resolveSudoku)

-- unsolved

eazyBoard :: [[Int]]
eazyBoard = [
    [1,0,8,9,0,0,0,4,0],
    [0,4,3,8,0,0,0,2,7],
    [0,0,0,0,0,0,0,0,0],
    [0,0,0,1,0,7,0,0,2],
    [0,0,9,0,4,2,0,0,6],
    [7,2,4,0,6,0,3,1,0],
    [0,0,1,6,0,5,9,7,4],
    [8,0,0,7,0,0,0,0,1],
    [0,6,7,2,1,9,8,0,5]
    ]

mediumBoard :: [[Int]]
mediumBoard = [
    [3,0,0,6,0,2,0,0,4],
    [0,0,0,0,7,9,8,0,3],
    [0,1,0,0,0,0,7,0,0],
    [0,0,0,0,6,0,0,0,2],
    [0,4,1,7,0,8,0,0,0],
    [7,0,6,0,0,0,4,0,0],
    [9,0,7,2,8,5,3,4,0],
    [0,8,0,0,0,0,2,9,0],
    [1,2,3,0,4,7,5,6,0]
    ]

hardBoard :: [[Int]]
hardBoard = [
    [9,0,2,3,0,0,8,0,1],
    [0,1,5,0,0,0,3,0,0],
    [4,0,7,0,0,0,0,5,6],
    [0,0,8,0,0,7,1,0,2],
    [0,0,0,0,2,6,5,9,0],
    [0,0,0,5,8,0,0,0,4],
    [0,0,0,0,0,0,0,0,0],
    [0,2,0,6,0,0,0,0,5],
    [0,7,9,2,0,5,0,0,0]
    ]

expertBoard :: [[Int]]
expertBoard = [
    [0,2,6,3,1,0,0,0,0],
    [0,0,0,7,0,2,3,0,0],
    [3,1,0,0,0,0,2,4,0],
    [7,9,8,4,3,1,0,0,0],
    [0,0,0,0,7,0,0,9,8],
    [0,0,0,0,9,0,0,0,0],
    [5,0,9,0,2,3,8,0,0],
    [0,6,0,0,0,0,9,0,0],
    [0,3,4,9,0,0,0,7,0]
    ]

masterBoard :: [[Int]]
masterBoard = [
    [8,0,0,0,0,9,0,0,5],
    [0,4,0,0,0,0,7,2,6],
    [0,0,0,0,3,0,8,0,0],
    [0,0,0,0,4,7,0,0,0],
    [0,3,0,0,5,0,0,0,0],
    [0,0,4,0,9,0,5,3,0],
    [0,0,0,0,7,0,0,0,0],
    [9,1,0,0,2,0,0,8,0],
    [0,2,0,0,0,5,9,0,0]
    ]

extremeBoard :: [[Int]]
extremeBoard = [
    [4,0,0,0,0,0,0,0,0],
    [0,0,0,2,1,8,0,7,0],
    [7,0,0,0,9,0,0,0,2],
    [0,0,6,0,3,0,8,0,4],
    [1,0,0,0,0,0,0,2,0],
    [0,0,5,0,0,7,0,0,0],
    [0,1,0,0,6,0,0,0,0],
    [0,6,0,0,8,5,0,0,0],
    [0,0,9,0,0,0,0,0,1]
    ]

-- solved

eazyBoardSolved :: [[Int]]
eazyBoardSolved = [
    [1,7,8,9,2,6,5,4,3],
    [9,4,3,8,5,1,6,2,7],
    [6,5,2,4,7,3,1,9,8],
    [3,8,6,1,9,7,4,5,2],
    [5,1,9,3,4,2,7,8,6],
    [7,2,4,5,6,8,3,1,9],
    [2,3,1,6,8,5,9,7,4],
    [8,9,5,7,3,4,2,6,1],
    [4,6,7,2,1,9,8,3,5]
    ]

mediumBoardSolved :: [[Int]]
mediumBoardSolved = [
    [3,7,8,6,1,2,9,5,4],
    [6,5,2,4,7,9,8,1,3],
    [4,1,9,8,5,3,7,2,6],
    [8,9,5,3,6,4,1,7,2],
    [2,4,1,7,9,8,6,3,5],
    [7,3,6,5,2,1,4,8,9],
    [9,6,7,2,8,5,3,4,1],
    [5,8,4,1,3,6,2,9,7],
    [1,2,3,9,4,7,5,6,8]
    ]

hardBoardSolved :: [[Int]]
hardBoardSolved = [
    [9,6,2,3,5,4,8,7,1],
    [8,1,5,7,6,2,3,4,9],
    [4,3,7,8,1,9,2,5,6],
    [6,5,8,4,9,7,1,3,2],
    [7,4,3,1,2,6,5,9,8],
    [2,9,1,5,8,3,7,6,4],
    [5,8,6,9,3,1,4,2,7],
    [3,2,4,6,7,8,9,1,5],
    [1,7,9,2,4,5,6,8,3]
    ]

expertBoardSolved :: [[Int]]
expertBoardSolved = [
    [9,2,6,3,1,4,7,8,5],
    [4,8,5,7,6,2,3,1,9],
    [3,1,7,8,5,9,2,4,6],
    [7,9,8,4,3,1,6,5,2],
    [6,4,3,2,7,5,1,9,8],
    [1,5,2,6,9,8,4,3,7],
    [5,7,9,1,2,3,8,6,4],
    [8,6,1,5,4,7,9,2,3],
    [2,3,4,9,8,6,5,7,1]
    ]

masterBoardSolved :: [[Int]]
masterBoardSolved = [
    [8,7,1,2,6,9,3,4,5],
    [3,4,9,5,1,8,7,2,6],
    [2,5,6,7,3,4,8,9,1],
    [5,9,8,3,4,7,1,6,2],
    [1,3,2,8,5,6,4,7,9],
    [7,6,4,1,9,2,5,3,8],
    [6,8,3,9,7,1,2,5,4],
    [9,1,5,4,2,3,6,8,7],
    [4,2,7,6,8,5,9,1,3]
    ]

extremeBoardSolved :: [[Int]]
extremeBoardSolved = [
    [4,8,2,7,5,3,9,1,6],
    [6,9,3,2,1,8,4,7,5],
    [7,5,1,4,9,6,3,8,2],
    [2,7,6,5,3,1,8,9,4],
    [1,3,8,6,4,9,5,2,7],
    [9,4,5,8,2,7,1,6,3],
    [5,1,4,9,6,2,7,3,8],
    [3,6,7,1,8,5,2,4,9],
    [8,2,9,3,7,4,6,5,1]
    ]

-- tests

testEasy :: Test
testEasy = TestCase (assertEqual "Resolving Easy Board" eazyBoardSolved (resolveSudoku eazyBoard))

testMedium :: Test
testMedium = TestCase (assertEqual "Resolving Medium Board" mediumBoardSolved (resolveSudoku mediumBoard))

testHard :: Test
testHard = TestCase (assertEqual "Resolving Hard Board" hardBoardSolved (resolveSudoku hardBoard))

testExpert :: Test
testExpert = TestCase (assertEqual "Resolving Expert Board" expertBoardSolved (resolveSudoku expertBoard))

testMaster :: Test
testMaster = TestCase (assertEqual "Resolving Master Board" masterBoardSolved (resolveSudoku masterBoard))

testExtreme :: Test
testExtreme = TestCase (assertEqual "Resolving Extreme Board" extremeBoardSolved (resolveSudoku extremeBoard))

resolvingBoards :: Test
resolvingBoards = TestList [testEasy, testMedium, testHard, testExpert, testMaster, testExtreme]