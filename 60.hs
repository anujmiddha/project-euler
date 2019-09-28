-- Optimized isPrime from https://stackoverflow.com/a/4572515/493321
primes :: [Int]
primes = sieve (2 : 3 : possible [1..]) where
     sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
     possible (x:xs) = 6*x-1 : 6*x+1 : possible xs

isPrime :: Int -> Bool
isPrime n = shortCircuit || (not $ any divisible $ takeWhile inRangeOf primes) where
    shortCircuit = elem n [2,3] || (n < 25 && ((n-1) `mod` 6 == 0 || (n+1) `mod` 6 == 0))
    divisible y = n `mod` y == 0
    inRangeOf y = y * y <= n
-- End

-- Generates the list of primes less than a
primesBelow :: Int -> [Int]
primesBelow a = filter isPrime [2..a]

-- Check if two numbers, when concatenated, produce primes
concatPrime :: Int -> Int -> Bool
concatPrime a b = (concatTest a b) && concatTest b a
    where
        concatTest :: Int -> Int -> Bool
        concatTest a b = isPrime (read . concat . map show $ [a, b] :: Int)

-- Since we are building the chain by adding a prime to a valid chain, only need to
-- test concatPrime for the last element with the rest of the list
validPrimes :: [Int] -> Bool
validPrimes [x] = True
validPrimes xs = (all (\a -> concatPrime a lastElem) oldChain)
    where
        lastElem = last xs
        oldChain = init xs

-- Takes an initial chain, list of primes that it will scan, an originalChain to
-- resume search after an element has been rejected. Returns a list of chains that
-- match the criteria
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