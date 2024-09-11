module ResolverTests.Base (baseTests4x4, baseTests9x9) where

import ResolverTests.Consts (board9x9, board4x4)

import Resolver(findNumMisPerLine, findNumMisPerCollumn, findNumMisPerQuadrant, generateQuadrants, calculateDimension)

import Test.HUnit (Test(..), assertEqual)

quadrants4x4 :: [[Int]]
quadrants4x4 = [[1,0,0,4], [3,0,0,2], [2,0,0,1], [4,1,0,0]]

numsMisPerLine4x4 :: [[Int]]
numsMisPerLine4x4 = [[2, 4], [1, 3], [3], [2,3,4]]

numsMisPerCollumn4x4 :: [[Int]]
numsMisPerCollumn4x4 = [[3,4], [2,3], [1,2], [3,4]]

numsMisPerQuadrant4x4 :: [[Int]]
numsMisPerQuadrant4x4 = [[2,3], [1,4], [3,4], [2,3]]

verifyCalculateDimension4x4 :: Test
verifyCalculateDimension4x4 = TestCase (
    assertEqual "Testing Dimension's Calculation" 2 ((calculateDimension . head) board4x4)
    )

verifyQuadrant4x4 :: Test
verifyQuadrant4x4 = TestCase (
    assertEqual "Testing Quadrant's Generation" quadrants4x4 (generateQuadrants 0 board4x4) 
    )

verifyNumMisPerLine4x4 :: Test
verifyNumMisPerLine4x4 = TestCase (
    assertEqual "Testing finding missing numbers in Lines" numsMisPerLine4x4 (findNumMisPerLine board4x4) 
    )

verifyNumMisPerCollumn4x4 :: Test
verifyNumMisPerCollumn4x4 = TestCase (
    assertEqual "Testing finding missing numbers in Collumns" numsMisPerCollumn4x4 (findNumMisPerCollumn board4x4) 
    )

verifyNumMisPerQuadrant4x4 :: Test
verifyNumMisPerQuadrant4x4 = TestCase (
    assertEqual "Testing finding missing numbers in Quadrants" numsMisPerQuadrant4x4 (findNumMisPerQuadrant board4x4) 
    )

baseTests4x4 :: Test
baseTests4x4 = TestList [verifyCalculateDimension4x4, verifyQuadrant4x4, verifyNumMisPerLine4x4, verifyNumMisPerCollumn4x4, verifyNumMisPerQuadrant4x4]

quadrants9x9 :: [[Int]]
quadrants9x9 = [
    [0,8,5,4,0,0,0,0,0],
    [0,0,0,8,0,0,4,0,0],
    [0,0,3,1,2,0,0,5,0],
    [7,3,8,0,5,4,0,6,9],
    [0,5,0,2,9,3,7,1,8],
    [9,0,2,6,8,7,4,3,0],
    [5,2,0,0,0,1,0,4,7],
    [9,0,1,0,0,2,5,8,0],
    [0,0,8,5,6,4,2,0,0]
    ]

numsMisPerLine9x9 :: [[Int]]
numsMisPerLine9x9 = [
    [1,2,4,6,7,9], [3,5,6,7,9],
    [1,2,3,6,7,8,9], [1,4,6], 
    [1], [2,5], [3,4,6,7], 
    [3,7,8,9], [1,3,6,9]
    ]

numsMisPerCollumn9x9 :: [[Int]]
numsMisPerCollumn9x9 = [
    [1,2,3,6,8,9], [1,7,9], 
    [2,3,6], [1,3,6], [2,3,4,6,7], 
    [4,5,6,7,9], [3,7,8], [1,4,7,9],
    [1,5,6,9]
    ]

numsMisPerQuadrant9x9 :: [[Int]]
numsMisPerQuadrant9x9 = [
    [1,2,3,6,7,9], 
    [1,2,3,5,6,7,9],
    [4,6,7,8,9], [1,2], 
    [4,6], [1,5], [3,6,8,9],
    [3,4,6,7], [1,3,7,9]
    ]

verifyCalculateDimension9x9 :: Test
verifyCalculateDimension9x9 = TestCase (
    assertEqual "Testing Dimension's Calculation" 3 ((calculateDimension . head) board9x9)
    )

verifyQuadrant9x9 :: Test
verifyQuadrant9x9 = TestCase (
    assertEqual "Testing Quadrant's Generation" quadrants9x9 (generateQuadrants 0 board9x9) 
    )

verifyNumMisPerLine9x9 :: Test
verifyNumMisPerLine9x9 = TestCase (
    assertEqual "Testing finding missing numbers in Lines" numsMisPerLine9x9 (findNumMisPerLine board9x9) 
    )

verifyNumMisPerCollumn9x9 :: Test
verifyNumMisPerCollumn9x9 = TestCase (
    assertEqual "Testing finding missing numbers in Collumns" numsMisPerCollumn9x9 (findNumMisPerCollumn board9x9) 
    )

verifyNumMisPerQuadrant9x9 :: Test
verifyNumMisPerQuadrant9x9 = TestCase (
    assertEqual "Testing finding missing numbers in Quadrants" numsMisPerQuadrant9x9 (findNumMisPerQuadrant board9x9) 
    )

baseTests9x9 :: Test
baseTests9x9 = TestList [verifyCalculateDimension9x9, verifyQuadrant9x9, verifyNumMisPerLine9x9, verifyNumMisPerCollumn9x9, verifyNumMisPerQuadrant9x9]