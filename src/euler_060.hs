-- Code to generate primes is from Haskel Wiki
-- http://www.haskell.org/haskellwiki/Prime_numbers
primes :: (Integral a) => [a]
primes = 2: 3: sieve (tail primes) [5,7..]
    where
        sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
            where (h,~(_:t)) = span (< p*p) xs

getPrimesUntil n = takeWhile (<= n) primes

isPrime n = elem n (getPrimesUntil n)

--------------------------------------------------------------------------------

isPrimesTogether a b = and [isPrimesTogether' a b, isPrimesTogether' b a]
    where isPrimesTogether' a b = isPrime (read (show a ++ show b))

isPrimesTogetherList [x] = True
isPrimesTogetherList (x:xs) =
    and [isPrimesTogether x y | y <- xs] &&
    isPrimesTogetherList xs
