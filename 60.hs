isPrime :: Int -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime a = not (any (\x -> a `mod` x == 0) [2..root])
    where
        root = truncate (sqrt (fromIntegral a))

concatPrime :: Int -> Int -> Bool
concatPrime a b = (concatTest a b) && concatTest b a
    where
        concatTest :: Int -> Int -> Bool
        concatTest a b = isPrime (read . concat . map show $ [a, b] :: Int)

validPrimes :: [Int] -> Bool
validPrimes [x] = True
validPrimes (x:xs) = (all (\a -> concatPrime a x) xs) && validPrimes xs

ans :: [Int] -> [Int] -> [Int] -> [[Int]]
ans chain [] originalList
        | length chain == 5 = chain : nextChain
        | length chain == 0 = [[]]
        | otherwise = nextChain
        where
            nextChain = ans (init chain) (dropWhile (<= (last chain)) originalList) originalList
ans chain (x:primes) originalList
        | length chain == 5 = chain : nextChain
        | validPrimes (chain ++ [x]) = ans (chain ++ [x]) primes originalList
        | otherwise = ans chain primes originalList
        where
            nextChain = ans (init chain) (dropWhile (<= (last chain)) originalList) originalList

finalAns = ans [] ps ps
        where
            ps = primesBelow 20000

primesBelow a = filter isPrime [2..a]