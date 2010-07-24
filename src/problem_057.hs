-- Project Euler problem #57

-- Calculates approximated square root of 2 as a fraction.
-- Arguments:
-- -- n: number of iterations
-- Returns: tuple (numerator, denominator)
sqrt2 0 = (1, 1)
sqrt2 n = (a + 2 * b, a + b) where (a, b) = sqrt2 (n - 1)

-- Returns True if fraction's numerator has more digits than its denominator
hasLongerNumerator (a, b) = length (show a) > length (show b)

-- Counts how much fractions from the list have numerator longer than denominator
countLongerNumerators fs = sum [1 | f <- fs, hasLongerNumerator f]

-- Solves the problem
solveProblem = countLongerNumerators [sqrt2 n | n <- [1..1000]]
