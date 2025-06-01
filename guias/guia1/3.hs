--I
sumFold :: [Int] -> Int
sumFold = foldr (+) 0

elemFold :: Eq a => a -> [a] -> Bool
elemFold n = foldr (\x acc -> (x == n) || acc) False

concatFold :: [a] -> [a] -> [a]
concatFold (xs) (ys) = foldr (:) ys xs

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold f = foldr (\x rec -> if f x then x : rec else rec) []

mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr (\x rec -> f x : rec) []

--II
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun esMejor = foldr1 (\x rec -> if x `esMejor` rec then x else rec)

--III
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\acc a -> if null acc then [a] else acc ++ [a + last acc]) []

--IV
sumaAlternada :: Num a => [a] -> a
sumaAlternada = foldr (-) 0

sumaAlternada2 :: Num a => [a] -> a --hace lo mismo que sumaAlternada
sumaAlternada2 = foldr1 (-)

--V
