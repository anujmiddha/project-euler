primes :: [Integer]
primes = 2 : sieve primes [3,5..] where
    sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
        where (h, t) = span (< p*p) xs

remainder :: Int -> Integer
remainder a = rem ((p - 1) ^ a + (p + 1) ^ a) (p * p)
  where
    p = primes!!(a-1)

answer :: Int
answer = head (filter validVal [7037..])
  where
    validVal a = remainder a > 10^10
