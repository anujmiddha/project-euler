-- To calculate the shortest path, start filling up the shortest paths in a matrix.
-- Starting from the last element, sweet the next diagonal row. 

import System.Environment
import Data.List.Split

main = do
    s <- readFile "p081_matrix.txt"
    print (calculateAnswer s)

calculateAnswer :: [Char] -> Int
calculateAnswer input = head (head (shortestPaths matrix (length (head matrix) - 1) (length matrix - 1) (preCalculatedPath (length matrix))))
    where
        matrix = parseMatrix input
        parseMatrix input = map parseLine (lines input)
        parseLine line = map parseInt (splitOn "," line)
        parseInt word = read word :: Int
        preCalculatedPath size = take size (repeat (take size (repeat (0))))

shortestPaths :: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
shortestPaths matrix x y preCalculatedPath
    | x == 0 && y == 0 = updatedPaths
    | otherwise        = shortestPaths matrix (fst nextPosition) (snd nextPosition) updatedPaths
    where
        updatedPaths = updatePosition matrix x y preCalculatedPath
        nextPosition 
            | x == 0                   = (y-1, 0)
            | y == (length matrix - 1) = (length (head matrix) - 1, x-1)
            | otherwise                = (x-1, y+1)

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
            | y == (length matrix - 1)                                = element + pathAt (x+1) y
            | x == (length (head matrix) - 1)                         = element + pathAt x (y+1)
            | otherwise                                               = element + min (pathAt x (y+1)) (pathAt (x+1) y)
            where
                element = (matrix !! y) !! x