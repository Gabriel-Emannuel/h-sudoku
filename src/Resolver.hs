module Resolver (resolveSudoku) where
import Data.List (transpose)
import Data.Maybe (isNothing)
import GHC.OldList (intersect, find)

resolveSudoku :: [[Int]] -> [[Int]]
resolveSudoku board
    | not (any (elem 0) board) = board
    | boardFilled /= board = resolveSudoku boardFilled
    | otherwise = tryFillBoard dimension allIntersections board
    where
        dimension = (round . sqrt . convertFloat . length ) board
        allIntersections = missingNumbersLineColsQuadrants dimension board
        possibleValues = identifyPossibleValues allIntersections
        boardFilled = fillBoard board possibleValues

convertFloat :: Int -> Float
convertFloat v = fromIntegral v :: Float

--- 

tryFillBoard :: Int -> [[[Int]]] -> [[Int]] -> [[Int]]
tryFillBoard dimension intersections board = 
    tryFill (validIntersections !! y !! x) (x,y) board
    where
        validIntersections = zipWith (zipWith (validateIntersection dimension)) board intersections
        (x,y) = findIntersectionCoordenate validIntersections

tryFill :: [Int] -> (Int, Int) -> [[Int]] -> [[Int]]
tryFill [] _ _ = []
tryFill (possibleValue:possibleValues) (x,y) board 
    | null sudokuResolved = tryFill possibleValues (x,y) board
    | otherwise = sudokuResolved 
    where
        sudokuResolved = resolveSudoku $ insert y (insert x possibleValue (board !! y)) board

insert :: Int -> t -> [t] -> [t]
insert index elem array = take index array ++ [elem] ++ drop (succ index) array

validateIntersection :: Int -> Int -> [Int] -> [Int]
validateIntersection _ 0 intersections = intersections
validateIntersection dimension _ _     = [0..dimension^2]

findIntersectionCoordenate :: [[[Int]]] -> (Int, Int)
findIntersectionCoordenate allIntersections = (x,y)
    where
        allLengths = map (map length) allIntersections
        minimumLengths = map minimum allLengths
        Just y = find (== minimum minimumLengths) minimumLengths
        Just x = find (== minimum minimumLengths) (allLengths !! y)

---

fillBoard :: [[Int]] -> [[Maybe Int]] -> [[Int]]
fillBoard = zipWith (zipWith fillValue)

fillValue :: Int -> Maybe Int -> Int
fillValue 0 (Just value) = value
fillValue elem _ = elem

---

identifyPossibleValues :: [[[Int]]] -> [[Maybe Int]]
identifyPossibleValues = map (map identifyPossibleValue)

identifyPossibleValue :: [Int] -> Maybe Int
identifyPossibleValue [value] = Just value
identifyPossibleValue _       = Nothing

---

missingNumbersLineColsQuadrants :: Int -> [[Int]] -> [[[Int]]]
missingNumbersLineColsQuadrants dimension board =
    intersectionQuadrantsQuadrantLineCols [[line `intersect` col | col <- numsCol] | line <- numsLine] (generateQuadrantsMatrix dimension numsQuadrant)
    where
        numsLine = missingLines board
        numsCol = missingCols board
        numsQuadrant = missingQuadrants dimension board

intersectionQuadrantsQuadrantLineCols :: [[[Int]]] -> [[[Int]]] -> [[[Int]]]
intersectionQuadrantsQuadrantLineCols = zipWith (zipWith intersect)

generateQuadrantsMatrix :: Int -> [[Int]] -> [[[Int]]]
generateQuadrantsMatrix _ [] = []
generateQuadrantsMatrix dimension quadrants =
    replicate dimension (concatMap (replicate dimension) (take dimension quadrants)) ++ generateQuadrantsMatrix dimension (drop dimension quadrants)
---

missingLines :: [[Int]] -> [[Int]]
missingLines = map missingNumbers

missingCols :: [[Int]] -> [[Int]]
missingCols = map missingNumbers . transpose

missingQuadrants :: Int -> [[Int]] -> [[Int]]
missingQuadrants dimension = map missingNumbers . generateQuadrants dimension

missingNumbers :: [Int] -> [Int]
missingNumbers nums = [number | number <- [1..length nums], number `notElem` nums]

---

generateQuadrants :: Int -> [[Int]] -> [[Int]]
generateQuadrants _ [] = []
generateQuadrants dimension board =
    divide dimension (take dimension board) ++ generateQuadrants dimension (drop dimension board)

divide :: Int -> [[Int]] -> [[Int]]
divide dimension board
    | all null board = []
    | otherwise = concatMap (take dimension) board : divide dimension (map (drop dimension) board)