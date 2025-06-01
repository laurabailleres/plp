--I
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat [] [] = []
sumaMat (x:xs) (y:ys) = zipWith (+) x y : sumaMat xs ys

--II
