--I
genListaE :: a -> (a -> a) -> Integer -> [a]
genListaE i f 0 = []
genListaE i f n = (f i) : genListaE (f i) f (n-1)

--II
--desdeHasta :: (Int,Int) -> [Int]
--desdeHasta (x,y) = genLista ...