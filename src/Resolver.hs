module Resolver where

import Data.List(delete, intersect, transpose, elemIndex, mapAccumL)

resolveSudoku :: [[Int]] -> [[Int]]
resolveSudoku board
    | isResolved board = board
    | board /= boardFilled = resolveSudokuRecursive d numMisPerLine numMisPerCollumn numMisPerQuadrant boardFilled
    | otherwise = tryFillBoard d numMisPerLine numMisPerCollumn numMisPerQuadrant boardFilled
    where
        d = (calculateDimension . head) board
        numMisPerLine = findNumMisPerLine board
        numMisPerCollumn = findNumMisPerCollumn board
        numMisPerQuadrant = findNumMisPerQuadrant board
        boardFilled = fillBoard d (0,0) numMisPerLine numMisPerCollumn numMisPerQuadrant board

resolveSudokuRecursive :: Int -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
resolveSudokuRecursive d numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | isResolved board = board
    | board /= boardFilled = resolveSudokuRecursive d numMisPerLine numMisPerCollumn numMisPerQuadrant boardFilled
    | otherwise = tryFillBoard d numMisPerLine numMisPerCollumn numMisPerQuadrant boardFilled
    where
        boardFilled = fillBoard d (0,0) numMisPerLine numMisPerCollumn numMisPerQuadrant board

tryFillBoard :: Int -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
tryFillBoard d numMisPerLine numMisPerCollumn numMisPerQuadrant board = 
    tryFillBoardRecursive d (x,y) (intersectionQuadrant !! y !! x) numMisPerLine numMisPerCollumn numMisPerQuadrant board
    where
        intersectionLineCollumn = [[line `intersect` collumn | collumn <- numMisPerCollumn] | line <- numMisPerLine]
        intersectionQuadrant = intersectQuadrant d intersectionLineCollumn numMisPerQuadrant
        (x,y) = minimumIntersectionCoordenate board intersectionQuadrant

tryFillBoardRecursive :: Int -> (Int, Int) -> [Int] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
tryFillBoardRecursive _ _ [] _ _ _ _ = []
tryFillBoardRecursive d (x,y) (num: nums) numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | null newBoard = tryFillBoardRecursive d (x,y) nums numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | otherwise = resolveSudokuRecursive d newNumMisPerLine newNumMisPerCollumn newNumMisPerQuadrant (insert (x,y) num board)
    where
        newBoard = resolveSudokuRecursive d newNumMisPerLine newNumMisPerCollumn newNumMisPerQuadrant (insert (x,y) num board)
        newNumMisPerLine = updateNumsMiss y num numMisPerLine
        newNumMisPerCollumn = updateNumsMiss x num numMisPerCollumn
        newNumMisPerQuadrant = updateNumsMiss y num numMisPerQuadrant

minimumIntersectionCoordenate ::[[Int]] -> [[[Int]]] -> (Int, Int)
minimumIntersectionCoordenate board intersectionResult = (x,y)
    where
        takeIntersections :: [Int] -> [[Int]] -> [[Int]]
        takeIntersections = zipWith (\a b -> if a == 0 then b else [0..length board])

        intersectionsResult = zipWith takeIntersections board intersectionResult

        intersectionResultLength = map (map length) intersectionsResult

        minimumsIntersections = map minimum intersectionResultLength
        minimumIntersection = minimum minimumsIntersections

        Just y = elemIndex minimumIntersection minimumsIntersections
        Just x = elemIndex minimumIntersection (intersectionResultLength !! y)

intersectQuadrant :: Int -> [[[Int]]] -> [[Int]] -> [[[Int]]]
intersectQuadrant d previusIntersection numMisPerQuadrant = result
    where
        f1 :: Int -> [[Int]] -> (Int, [[Int]])
        f1 y lineIntersection =  (succ y, snd (mapAccumL (f2 y) 0 lineIntersection))
        
        f2 :: Int -> Int -> [Int] -> (Int, [Int])
        f2 y x intersection = (succ x, intersection `intersect` (numMisPerQuadrant !! witchQuadrant d (x, y)))
        
        result = snd (mapAccumL f1 0 previusIntersection) 

fillBoard :: Int -> (Int, Int) -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
fillBoard d (x,y) numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | y >= length board = board
    | x >= length board = fillBoard d (0, succ y) numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | board !! y !! x /= 0 || length numsPlace > 1 = fillBoard d (succ x, y) numMisPerLine numMisPerCollumn numMisPerQuadrant board
    | null numsPlace = []
    | otherwise = fillBoard d (succ x, y) newNumMisPerLine newNumMisPerCollumn newNumMisPerQuadrant (insert (x,y) num board)
        where            
            numsLine = numMisPerLine !! y
            numsCollumn = numMisPerCollumn !! x
            quadrant = witchQuadrant d (x,y)
            numsQuadrant = numMisPerQuadrant !! quadrant
            numsPlace = numsCollumn `intersect` numsLine `intersect` numsQuadrant
            [num] = numsPlace
            newNumMisPerLine = updateNumsMiss y num numMisPerLine
            newNumMisPerCollumn = updateNumsMiss x num numMisPerCollumn
            newNumMisPerQuadrant = updateNumsMiss quadrant num numMisPerQuadrant

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

witchQuadrant :: Int -> (Int, Int) -> Int
witchQuadrant baseDimension (x,y) = xQuad + yQuad
    where
        xQuad = x `div` baseDimension
        yQuad = (y `div` baseDimension) * baseDimension

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