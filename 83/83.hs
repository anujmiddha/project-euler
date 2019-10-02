-- To calculate the shortest path, start filling up the shortest paths in a matrix.
-- Starting from the last element, sweet the next diagonal row. 

import System.Environment
import Data.List.Split

main = do
    s <- readFile "p083_matrix.txt"
    print (head (head (calculateAnswer s)))

calculateAnswer :: [Char] -> [[Int]]
calculateAnswer input = shortestPaths matrix lastX lastY firstPass
    where
        firstPass = shortestPaths matrix lastX lastY emptyPaths
        matrix = parseMatrix input
        parseMatrix input = map parseLine (lines input)
        parseLine line = map parseInt (splitOn "," line)
        parseInt word = read word :: Int
        emptyPaths = let size = length matrix
                     in take size (repeat (take size (repeat (0))))
        lastX = length (head matrix) - 1
        lastY = length matrix - 1

shortestPaths :: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
shortestPaths matrix x y preCalculatedPath
    | x == 0 && y == 0 = updatedPaths
    | otherwise        = shortestPaths matrix (fst nextPosition) (snd nextPosition) updatedPaths
    where
        updatedPaths = updateNeighbours matrix ((x,y) : (neighbours matrix x y)) preCalculatedPath
        nextPosition 
            | x == 0                   = (y-1, 0)
            | y == (length matrix - 1) = (length (head matrix) - 1, x-1)
            | otherwise                = (x-1, y+1)

neighbours :: [[Int]] -> Int -> Int -> [(Int, Int)]
neighbours matrix x y = filter isValid [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]
    where
        isValid point = (pointX >= 0 && pointX <= lastX) && (pointY >= 0 && pointY <= lastY)
            where
                pointX = fst point
                pointY = snd point
        lastX = length (head matrix) - 1
        lastY = length matrix - 1

updateNeighbours :: [[Int]] -> [(Int, Int)] -> [[Int]] -> [[Int]]
updateNeighbours matrix [] preCalculatedPath = preCalculatedPath
updateNeighbours matrix (point:points) preCalculatedPath = updateNeighbours matrix points updatedPaths
    where
        updatedPaths = updatePosition matrix (fst point) (snd point) preCalculatedPath

updatePosition :: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
updatePosition matrix x y preCalculatedPath = preRows ++ [(preColumns ++ [shortestPath] ++ postCoumns)] ++ postRows
    where
        preRows = (take (y) preCalculatedPath)
        postRows = (drop (y+1) preCalculatedPath)
        preColumns = take (x) currentRow
        postCoumns = drop (x+1) currentRow
        currentRow = preCalculatedPath !! y
        pathAt x' y' = (preCalculatedPath !! y') !! x'
        shortestPath
            | y == (length matrix - 1) && x == (length (head matrix) - 1) = element
            | otherwise                                                   = element + foldr1 min (filter (>0) (map (\p -> pathAt (fst p) (snd p)) (neighbours matrix x y)))
            where
                element = (matrix !! y) !! x