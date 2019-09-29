import System.Environment
import Data.List.Split

main = do
    s <- readFile "p082_matrix.txt"
    print (calculateAnswer s)

calculateAnswer :: [Char] -> Int
calculateAnswer input = foldr1 min (map head (shortestPaths matrix (length (head matrix) - 1) 0 Down (preCalculatedPath (length matrix))))
    where
        matrix = parseMatrix input
        parseMatrix input = map parseLine (lines input)
        parseLine line = map parseInt (splitOn "," line)
        parseInt word = read word :: Int
        preCalculatedPath size = take size (repeat (take size (repeat (0))))

data Direction = Down | Up

shortestPaths :: [[Int]] -> Int -> Int -> Direction -> [[Int]] -> [[Int]]
shortestPaths matrix 0 0 Up preCalculatedPath = updatedPaths
    where
        updatedPaths = updatePosition matrix 0 0 Up preCalculatedPath
shortestPaths matrix x y direction preCalculatedPath
    | otherwise        = shortestPaths matrix (fst (snd (nextPosition direction))) (snd (snd (nextPosition direction))) (fst (nextPosition direction)) updatedPaths
    where
        updatedPaths = updatePosition matrix x y direction preCalculatedPath
        pathAt x' y' = (preCalculatedPath !! y') !! x'
        nextPosition Up
            | y == 0    = (Down, (x-1, y))
            | otherwise = (Up, (x, y-1))
        nextPosition Down
            | y == (length matrix - 1) = (Up, (x, y))
            | otherwise                = (Down, (x, y+1))

updatePosition :: [[Int]] -> Int -> Int -> Direction -> [[Int]] -> [[Int]]
updatePosition matrix x y direction preCalculatedPath = preRows ++ [(preColumns ++ [shortestPath direction] ++ postCoumns)] ++ postRows
    where
        preRows = (take (y) preCalculatedPath)
        postRows = (drop (y+1) preCalculatedPath)
        preColumns = take (x) currentRow
        postCoumns = drop (x+1) currentRow
        currentRow = preCalculatedPath !! y
        pathAt x' y' = (preCalculatedPath !! y') !! x'
        shortestPath Down
            | x == (length (head matrix) - 1) = element
            | y == 0                          = element + pathAt (x+1) y
            | otherwise                       = element + min (pathAt (x+1) y) (pathAt x (y-1))
            where
                element = (matrix !! y) !! x
        shortestPath Up
            | x == (length (head matrix) - 1) = element
            | y == (length matrix - 1)        = min (pathAt x y) (element + pathAt (x+1) y)
            | otherwise                       = min (pathAt x y) (element + min (pathAt (x+1) y) (pathAt x (y+1)))
            where
                element = (matrix !! y) !! x