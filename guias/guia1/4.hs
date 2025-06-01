--I
ponerEnPosicion :: Int -> a -> [a] -> [a]
ponerEnPosicion n e xs = (take n xs) ++ [e] ++ (drop n xs)

--permutaciones :: [a] -> [[a]]

--II
partes :: [a] -> [[a]]
partes = foldr (\x rec -> rec ++ map (x:) rec) [[]]

--III
prefijosE :: [a] -> [[a]]
prefijosE [] = [[]]
prefijosE (x:xs) = [] : map (x:) (prefijosE xs)

prefijos :: [a] -> [[a]]
prefijos = foldr (\x rec -> [] : map (x:) rec) [[]]

--IV
