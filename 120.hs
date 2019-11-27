import Data.List (elemIndex)

test :: Integer -> Integer -> Integer
test a n = rem ((a - 1) ^ n + (a + 1) ^ n) (a ^ 2)

tryAll :: Integer -> [Integer]
tryAll a = map (test a) [0..upper]
  where
    upper = a * 2

maxRemainder :: Integer -> Integer
maxRemainder a = maximum (tryAll a)

answer :: Integer
answer = sum (map maxRemainder [3..1000])
