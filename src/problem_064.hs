import Data.List (inits)

-- Utility functions
floatFloor = fromIntegral . floor
floorSqrt = floatFloor . sqrt
isSquare n = (floor $ sqrt n)^2 == round n
nonSquares = filter (not . isSquare) [1..]

getA n b c = (a, b1, c1)
    where
        m = floorSqrt n
        c1 = (n - b ^ 2) / c  -- Is it always integer?
        a = floatFloor $ (m + b) / c1
        b1 = a * c1 - b

getAs n b c = (a, b1, c1) : (getAs n b1 c1)
    where (a, b1, c1) = getA n b c

sqrtFractions n = (round m, repeatingASecuence)
    where
        m = floorSqrt n
        repeatingASecuence
            | m * m == n = []
            | otherwise  = map (\(x, _, _) -> round x) $ init $ head $
                           dropWhile (\x -> not $ elem (head x) (tail x)) $
                           dropWhile null $ inits $ getAs n m 1

solveFor n = length $ filter odd $ map (\i -> length $ snd $ sqrtFractions i) $ takeWhile (<=n) nonSquares

solveProblem = solveFor 10000
