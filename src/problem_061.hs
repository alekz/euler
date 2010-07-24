import Data.List (nub)

-- Generate n-th polygonal number of the specified type (3..8)
p :: Int -> Int -> Int
p 3 n = n * (n + 1) `div` 2      -- Triangle
p 4 n = n * n                    -- Square
p 5 n = n * (3 * n - 1) `div` 2  -- Pentagonal
p 6 n = n * (2 * n - 1)          -- Hexagonal
p 7 n = n * (5 * n - 3) `div` 2  -- Heptagonal
p 8 n = n * (3 * n - 2)          -- Octagonal

-- Find position of the number in the polynomal sequence of the specified type
-- ("reverse" p)
rp :: (Floating a) => Int -> a -> a
rp 3 m = 1 / 2 * (sqrt(8 * m + 1) - 1)
rp 4 m = sqrt m
rp 5 m = 1 / 6 * (sqrt(24 * m + 1) + 1)
rp 6 m = 1 / 4 * (sqrt(8 * m + 1) + 1)
rp 7 m = 1 / 10 * (sqrt(40 * m + 9) + 3)
rp 8 m = 1 / 3 * (sqrt(3 * m + 1) + 1)

-- Generate sequence of polygonal numbers of the specified type (3..8)
ps :: Int -> [Int]
ps i | 3 <= i && i <= 8 = map (p i) [1..]

-- Checks whether number is a polygonal number of the specified type (3..8)
isPolygonal :: Int -> Int -> Bool
isPolygonal i m = m == (p i $ round $ rp i $ fromIntegral m)

-- Checks whether number is a polygonal number of any type
isAnyPolygonal :: Int -> Bool
isAnyPolygonal m = or $ map (`isPolygonal` m) [3..8]

-- Returns number of different types of polygonal numbers from the list
countDifferentPolygonalTypes x = maximum $ map (length . nub) $ getPolygonalTypeSetsForList x
    where

        -- Returns list of all polygonal types of the number
        getPolygonalTypes m = filter (`isPolygonal` m) [3..8]

        -- Returns list of all polygonal types of the number
        getPolygonalTypesForList x = map getPolygonalTypes x

        getPolygonalTypeSetsForList x = getPolygonalTypeSets $ getPolygonalTypesForList x
            where
                getPolygonalTypeSets [x] = map (\x -> [x]) x
                getPolygonalTypeSets (y:ys) = [z:zs | z <- y, zs <- getPolygonalTypeSets ys]

--------------------------------------------------------------------------------

fourDigitPolygonals :: [Int]
fourDigitPolygonals = filter isAnyPolygonal [1000..9999]
fourDigitPolygonalsLists = map (\x -> [x]) fourDigitPolygonals

isConnected a b = a /= b && (drop ((length $ show a) - 2) $ show a) == (take 2 $ show b)

isCyclic x = isConnected (last x) (head x)

findAllCyclic i = filter isCyclic $ findConnectedForList i
    where
        findConnectedForList 1 = fourDigitPolygonalsLists
        findConnectedForList j = [z:y:ys | (y:ys) <- findConnectedForList (j - 1), z <- fourDigitPolygonals, isConnected z y]

findAllUniqueCyclic i = filter (\x -> (countDifferentPolygonalTypes x) == i) $ findAllCyclic i

solveProblem = map sum $ findAllUniqueCyclic 6
