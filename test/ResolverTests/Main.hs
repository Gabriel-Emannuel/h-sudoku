module ResolverTests.Main (mainTests) where

import ResolverTests.Consts (board9x9, board4x4, board4x4Filled, board9x9Filled)

import Resolver(findNumMisPerLine, findNumMisPerCollumn, findNumMisPerQuadrant, intersectQuadrant ,fillBoard, minimumIntersectionCoordenate)

import Test.HUnit (Test(..), assertEqual)

import Data.List(intersect)

--- pre config

numMisPerLine4x4 :: [[Int]]
numMisPerLine4x4 = findNumMisPerLine board4x4

numMisPerCol4x4 :: [[Int]]
numMisPerCol4x4 = findNumMisPerCollumn board4x4

numMisPerQuadrant4x4 :: [[Int]]
numMisPerQuadrant4x4 = findNumMisPerQuadrant board4x4

numMisPerLine9x9 :: [[Int]]
numMisPerLine9x9 = findNumMisPerLine board9x9

numMisPerCol9x9 :: [[Int]]
numMisPerCol9x9 = findNumMisPerCollumn board9x9

numMisPerQuadrant9x9 :: [[Int]]
numMisPerQuadrant9x9 = findNumMisPerQuadrant board9x9

intersection4x4LineCol :: [[[Int]]]
intersection4x4LineCol = [[[4],[2],[2],[4]],[[3],[3],[1],[3]],[[3],[3],[],[3]],[[3,4],[2,3],[2],[3,4]]]

fullIntersection4x4LineColQuadrant :: [[[Int]]]
fullIntersection4x4LineColQuadrant = [[[],[2],[],[4]],[[3],[3],[1],[]],[[3],[3],[],[3]],[[3,4],[3],[2],[3]]]

fullIntersection9x9LineColQuadrant :: [[[Int]]]
fullIntersection9x9LineColQuadrant = [
    [[1,2,6,9],[1,7,9],[2,6],[1,6],[2,6,7],[6,7,9],[7],[4,7,9],[6,9]],
    [[3,6,9],[7,9],[3,6],[3,6],[3,6,7],[5,6,7,9],[7],[7,9],[6,9]],
    [[1,2,3,6,9],[1,7,9],[2,3,6],[1,3,6],[2,3,6,7],[6,7,9],[7,8],[7,9],[6,9]],
    [[1],[1],[],[6],[4,6],[4,6],[],[1],[1]],
    [[1],[1],[],[],[],[],[],[1],[1]],
    [[2],[],[2],[],[],[],[],[],[5]],
    [[3,6],[],[3,6],[3,6],[3,4,6,7],[4,6,7],[3,7],[7],[]],
    [[3,8,9],[9],[3],[3],[3,7],[7],[3,7],[7,9],[9]],
    [[3,6,9],[9],[3,6],[3,6],[3,6],[6],[3],[1,9],[1,9]]]
---

--- Fill Board

test1 :: Test
test1 = TestCase (assertEqual "Filling Board 4x4" board4x4Filled (fillBoard 2 (0,0) numMisPerLine4x4 numMisPerCol4x4 numMisPerQuadrant4x4 board4x4))

test2 :: Test
test2 = TestCase (assertEqual "Filling Board 9x9" board9x9Filled (fillBoard 3 (0,0) numMisPerLine9x9 numMisPerCol9x9 numMisPerQuadrant9x9 board9x9))

fillBoardTests :: Test
fillBoardTests = TestList [test1, test2]

---

--- Intersection

test3 :: Test
test3 = TestCase (assertEqual "Verifing Generating Col Line Intersection" intersection4x4LineCol [[line `intersect` col | col <- numMisPerCol4x4] | line <- numMisPerLine4x4])

test4 :: Test
test4 = TestCase (assertEqual "Verifing Intersection Col Line Quadrant Intersection 4x4" fullIntersection4x4LineColQuadrant (intersectQuadrant 2 intersection4x4LineCol numMisPerQuadrant4x4))

test5 :: Test
test5 = TestCase (assertEqual "Verifing Intersection Col Line Quadrant Intersection 9x9" fullIntersection9x9LineColQuadrant (intersectQuadrant 3 [[line `intersect` col | col <- numMisPerCol9x9] | line <- numMisPerLine9x9] numMisPerQuadrant9x9))

intersectionsTests :: Test
intersectionsTests = TestList [test3, test4, test5]

---

--- Minimum Intersection

test6 :: Test
test6 = TestCase (assertEqual "Verifing Minimum Intersection 4x4" (1,0) (minimumIntersectionCoordenate board4x4 fullIntersection4x4LineColQuadrant)) 

test7 :: Test
test7 = TestCase (assertEqual "Verifing Minimum Intersection 9x9" (6,0) (minimumIntersectionCoordenate board9x9 fullIntersection9x9LineColQuadrant)) 


minimumIntersectionTests :: Test
minimumIntersectionTests = TestList [test6, test7]

---

---

mainTests :: Test
mainTests = TestList [fillBoardTests, intersectionsTests, minimumIntersectionTests]