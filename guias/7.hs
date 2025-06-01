--I
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f xs = map (uncurry f) xs

--II
armarPares :: [a] -> [b] -> [(a, b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x, y) : (armarPares xs ys)

--III
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f [] _ = []
mapDoble f xs ys = mapPares f (armarPares xs ys)