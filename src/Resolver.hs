module Resolver where

import Data.List(delete, intersect, transpose, elemIndex)

resolveSudoku :: [[Int]] -> [[Int]]
resolveSudoku board
    | isResolved board = board
    | board /= boardFilled = resolveSudokuRecursive numMisPerLine numMisPerCollumn numMisPerQuadrant boardFilled
    | otherwise = boardResolved
    where
        numMisPerLine = findNumMisPerLine board
        numMisPerCollumn = findNumMisPerCollumn board
        numMisPerQuadrant = findNumMisPerQuadrant board
        boardFilled = fillBoard (0,0) numMisPerLine numMisPerCollumn numMisPerQuadrant board
        boardResolved = tryFillBoard numMisPerLine numMisPerCollumn numMisPerQuadrant boardFilled

tryFillBoard :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
tryFillBoard numMisPerLine numMisPerCollumn numMisPerQuadrant board = 
    tryFillBoardRecursive (x,y) (intersectionQuadrant !! y !! x) numMisPerLine numMisPerCollumn numMisPerQuadrant board
    where
        intersectionLineCollumn = [[line `intersect` collumn | collumn <- numMisPerCollumn] | line <- numMisPerLine]
        intersectionQuadrant = intersectQuadrant (0,0) intersectionLineCollumn numMisPerQuadrant
        (x,y) = minimumIntersectionCoordenate intersectionQuadrant

tryFillBoardRecursive :: (Int, Int) -> [Int] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
tryFillBoardRecursive _ [] _ _ _ _ = []
tryFillBoardRecursive (x,y) (num: nums) numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | null newBoard = tryFillBoardRecursive (x,y) nums numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | otherwise = newBoard
    where
        newBoard = resolveSudokuRecursive newNumMisPerLine newNumMisPerCollumn newNumMisPerQuadrant (insert (x,y) num board)
        newNumMisPerLine = updateNumsMiss y num numMisPerLine
        newNumMisPerCollumn = updateNumsMiss x num numMisPerCollumn
        newNumMisPerQuadrant = updateNumsMiss y num numMisPerQuadrant

minimumIntersectionCoordenate :: [[[Int]]] -> (Int, Int)
minimumIntersectionCoordenate intersectionResult = (x, y)
    where
        intersectionResultLength = map (map length) intersectionResult
        minimumsIntersections = map minimum intersectionResultLength
        minimumIntersection = minimum minimumsIntersections
        Just y = elemIndex minimumIntersection minimumsIntersections
        Just x = elemIndex minimumIntersection (intersectionResultLength !! y)

intersectQuadrant :: (Int, Int) -> [[[Int]]] -> [[Int]] -> [[[Int]]]
intersectQuadrant (x,y) intersectResult numMisPerQuadrant
    | y > length intersectResult = intersectResult
    | x > (length . head) intersectResult = intersectQuadrant (0, succ y) intersectResult numMisPerQuadrant
    | otherwise = intersectQuadrant (succ x, y) (insert (x,y) newIntersection intersectResult) numMisPerQuadrant
    where
        quadrant = witchQuadrant (x,y) 
        newIntersection = intersectResult !! y !! x `intersect` (numMisPerQuadrant !! quadrant)

fillBoard :: (Int, Int) -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
fillBoard (x,y) numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | y >= length board = board
    | x >= length board = fillBoard (0, succ y) numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | board !! y !! x == 0 || length numsPlace > 1 = fillBoard (succ x, y) numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | null numsPlace = []
    | otherwise = fillBoard (succ x, y) newNumMisPerLine newNumMisPerCollumn newNumMisPerQuadrant (insert (x,y) num board)
        where
            numsLine = numMisPerLine !! y
            numsCollumn = numMisPerCollumn !! x
            quadrant = witchQuadrant (x,y)
            numsQuadrant = numMisPerQuadrant !! quadrant
            numsPlace = numsCollumn `intersect` numsLine `intersect` numsQuadrant
            [num] = numsPlace
            newNumMisPerLine = updateNumsMiss y num numMisPerLine
            newNumMisPerCollumn = updateNumsMiss x num numMisPerCollumn
            newNumMisPerQuadrant = updateNumsMiss y num numMisPerQuadrant

insert :: (Int, Int) -> t -> [[t]] -> [[t]]
insert (x,y) num board =
    prefixBoard ++ [prefixLine ++ [num] ++ sufixLine] ++ sufixBoard
    where
        prefixBoard = take y board
        sufixBoard = drop (succ y) board
        prefixLine = take x lineBoard
        sufixLine = drop (succ x) lineBoard
        lineBoard = board !! y

updateNumsMiss :: Int -> Int -> [[Int]] -> [[Int]]
updateNumsMiss t num numMis =
    take t numMis ++ [delete num (numMis !! t)] ++ drop (succ t) numMis

witchQuadrant :: (Int, Int) -> Int
witchQuadrant (x,y) = xQuad + yQuad
    where
        xQuad = x `div` 3
        yQuad = (y `div` 3) * 3

resolveSudokuRecursive :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
resolveSudokuRecursive numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | isResolved board = board
    | board /= boardFilled = resolveSudokuRecursive numMisPerLine numMisPerCollumn numMisPerQuadrant boardFilled
    | otherwise = boardResolved
    where
        boardFilled = fillBoard (0,0) numMisPerLine numMisPerCollumn numMisPerQuadrant board
        boardResolved = tryFillBoard numMisPerLine numMisPerCollumn numMisPerQuadrant boardFilled

isResolved :: [[Int]] -> Bool
isResolved = foldl (\acc row -> acc && notElem 0 row) True

findNumsMis :: [Int] -> [Int]
findNumsMis numbers = [n | n <- [1..length numbers], n `notElem` numbers]

findNumMisPerLine :: [[Int]] -> [[Int]]
findNumMisPerLine = map findNumsMis

findNumMisPerCollumn :: [[Int]] -> [[Int]]
findNumMisPerCollumn = map findNumsMis . transpose

findNumMisPerQuadrant :: [[Int]] -> [[Int]]
findNumMisPerQuadrant board = map findNumsMis (generateQuadrants 0 board)

generateQuadrants :: Int -> [[Int]] -> [[Int]]
generateQuadrants _ [] = []
generateQuadrants x board
    | x >= lengthBoard = generateQuadrants 0 (drop dimension board)
    | otherwise = (concatMap (take dimension . drop x) . take dimension) board : generateQuadrants (x + dimension) board
    where
        lengthBoard = (length . head ) board
        dimension = (calculateDimension . head) board

calculateDimension :: [Int] -> Int
calculateDimension = round . sqrt . convertFloat . length

convertFloat :: Int -> Float
convertFloat = fromIntegral