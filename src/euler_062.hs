import Data.List (sort, group)

-- Infinite list of cubes
cubes :: [Integer]
cubes = map (^3) [1..]

-- List of all cubes with the specified length
cubesByLength :: Int -> [Integer]
cubesByLength n = dropWhile (< 10 ^ (n - 1)) $ takeWhile (< 10 ^ n) cubes

-- Checks two numbers whether they're permutations of each other
arePermutations :: Integer -> Integer -> Bool
arePermutations a b = (sort $ show a) == (sort $ show b)

-- Returns list of all permutations for which there are exactly "len" cubes of length "n"
basePermutations :: Int -> Int -> [Integer]
basePermutations len n =
        -- Convert list of cubes to list of strings; digits in strings are sorted
    let cubeStrings = map (\x -> reverse $ sort $ show x) $ cubesByLength n
        -- Group the equal strings together, keep only those groups of necessary length
        cubeStringGroups = filter (\x -> length x == len) $ group $ sort $ cubeStrings
        -- Convert every group of equal strings to a single integer
    in  map (\x -> (read $ head x) :: Integer) cubeStringGroups

{-
-- The same function as an ugly one-liner
basePermutations len n = map (\x -> ((read $ head x) :: Integer)) $ filter (\x -> length x == len) $ group $ sort $ map (\x -> reverse $ sort $ show x) $ cubesByLength n
-}

-- Solves problem for any given number of permutations
solveForPermutationSize :: Int -> Integer
solveForPermutationSize len =
        -- Finds base permutations of length len for the smallest possibe length of cubes
    let ps = head $ dropWhile null $ map (basePermutations len) [1..]
        -- Gets all cubes for that length
        cs = cubesByLength $ length $ show $ head ps
        -- Keeps only those cubes which are permutations of the base
        -- permutations, then finds the smallest one
    in  minimum [c | c <- cs, p <- ps, arePermutations c p]

-- Solves the original problem
solveProblem :: Integer
solveProblem = solveForPermutationSize 5
