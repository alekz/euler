import Primes

-- List of diagonal number for a given iteration
diagonals 1 = [1]
diagonals n = map (\i -> (2 * n - 1) ^ 2 - 2 * i * (n - 1)) [3,2,1,0]

-- [(iteration, primes, total)]
ratios' = (1, 0, 1) : map (\(n, p, c) -> (n + 1, p + pn (n + 1), c + 4)) ratios'
    where pn n = length $ filter isPrime $ diagonals n
ratios = map (\(i, p, c) -> (i, (fromIntegral p) / (fromIntegral c))) ratios'

-- Returns size of the square
solveFor k = j * 2 - 1 where j = fst $ head $ dropWhile (\(i, r) -> r > k) $ tail ratios

solveProblem = solveFor 0.1
