module Fundamentals (tests) where

import Test.HUnit (Test (TestList, TestCase), assertEqual)

import Resolver (missingLines, missingCols, missingQuadrants, missingNumbersLineColsQuadrants, fillBoard, identifyPossibleValues, resolveSudoku)

-- 4x4
boardSolved :: [[Int]]
boardSolved = [
    [1,2,3,4],
    [4,3,2,1],
    [2,4,1,3],
    [3,1,4,2]
    ]

boardUnsolved :: [[Int]]
boardUnsolved = [
    [1,0,0,4],
    [0,3,2,0],
    [0,4,1,0],
    [0,0,4,2]
    ]

boardFilled :: [[Int]]
boardFilled = [
    [1,2,3,4],
    [4,3,2,1],
    [0,4,1,3],
    [3,1,4,2]
    ]

lineS :: [[Int]]
lineS = [ [2,3], [1,4], [2,3], [1,3] ]

collumns :: [[Int]]
collumns = [ [2,3,4], [1,2], [3], [1,3] ]

quadrants :: [[Int]]
quadrants = [ [2,4], [1,3], [1,2,3], [3] ]

intersections :: [[[Int]]]
intersections = [ 
    [ [2], [2], [3], [3] ], 
    [ [4], [], [], [1] ], 
    [ [2,3], [2], [3], [3] ], 
    [ [3], [1], [3], [3] ] 
    ]

-- Test

testLines :: Test
testLines = TestCase (assertEqual "Lines Missing" lineS (missingLines boardUnsolved))

testCollumns :: Test
testCollumns = TestCase (assertEqual "Collumns Missing" collumns (missingCols boardUnsolved))

testQuadrants :: Test
testQuadrants = TestCase (assertEqual "Quadrants Missing" quadrants (missingQuadrants 2 boardUnsolved))

testIntersections :: Test
testIntersections = TestCase (assertEqual "Possible values per place" intersections (missingNumbersLineColsQuadrants 2 boardUnsolved))

testSimpleFill :: Test
testSimpleFill = TestCase (assertEqual "Filling One time" boardFilled (fillBoard boardUnsolved possibleValues))
    where
        possibleValues = identifyPossibleValues boardUnsolved intersections

testSimpleResolve :: Test
testSimpleResolve = TestCase (assertEqual "Simple Resolving" boardSolved (resolveSudoku boardUnsolved))

tests :: Test
tests = TestList [testLines, testCollumns, testQuadrants, testIntersections, testSimpleFill, testSimpleResolve]