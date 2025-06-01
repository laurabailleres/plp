recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

--a
sacarUnaE :: Eq a => a -> [a] -> [a]
sacarUnaE _ [] = []
sacarUnaE n (x:xs) = if (x == n) then xs else x:(sacarUnaE n xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n = recr (\x xs rec -> if (x == n) then xs else x:rec) []

--b
--un esquema de recursión estructural no es el adecuado para resolver 
--este problema ya que necesito seguir usando la lista (xs) para operar.

--c
insertarOrdenadoE :: Ord a => a -> [a] -> [a]
insertarOrdenadoE n [] = [n]
insertarOrdenadoE n (x:xs) = if (n > x) then x:insertarOrdenadoE n xs else n:x:xs

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n = recr (\x xs rec -> if (n > x) then x:rec else n:x:xs) [n]