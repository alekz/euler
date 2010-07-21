module Primes
( primes
, isPrime
, getPrimesUntil
) where

-- Code to generate primes is from Haskel Wiki
-- http://www.haskell.org/haskellwiki/Prime_numbers

primes :: (Integral a) => [a]
primes = 2: 3: sieve (tail primes) [5,7..]
    where
        sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
            where (h,~(_:t)) = span (< p*p) xs

isPrime n = n > 1 && n == head (primeFactors n)

primeFactors 1 = []
primeFactors n = go n primes
    where
        go n ps@(p:pt)
            | p*p > n        = [n]
            | n `rem` p == 0 = p : go (n `quot` p) ps
            | otherwise      = go n pt

getPrimesUntil n = takeWhile (<= n) primes
