--I
foldNat :: (Int -> Int) -> Int -> Int -> Int
foldNat _ c 0 = c 
foldNat f c n = f (foldNat f c (n-1))

--II
potenciaE :: Int -> Int -> Int
potenciaE _ 0 = 1
potenciaE n m = n * potenciaE n (m-1)

potencia :: Int -> Int -> Int
potencia n m = foldNat (*n) 1 m
