-- Approximated square root of 2
sqrt2 :: (Integral a, Floating b) => a -> b
sqrt2 0 = 1.0
sqrt2 n = 1 + 1 / (1 + sqrt2 (n - 1))

{-- Approximated square root of 2 (example of alternate declaration)
sqrt2 n
    | n == 0    = 1.0
    | otherwise = 1 + 1 / (1 + sqrt2 (n - 1))
-}
