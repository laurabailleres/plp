iv. ∀ xs::[a] . ∀ f::(a->b) . length (map f xs) = length xs

hago inducción estructural en listas

     length :: [a] -> Int
{L0} length [] = 0
{L1} length (x:xs) = 1 + length xs

      map :: (a -> b) -> [a] -> [b]
{map} foldr (\x rec -> f x : rec) []

caso base P([])
length (map f []) = length [] ={map}
length (foldr (\x rec -> f x : rec) [] []) = length [] ={foldr0}
length [] = length [] ={L0}
0 = length [] ={L0}
0 = 0 si..

paso inductivo  P(xs) ⇒ P(x:xs)

h.i. length (map f xs) = length xs
     length (foldr (\x rec -> f x : rec) [] xs) = length xs
q.v.q. length (map f (x:xs)) = length (x:xs)

length (map f (x:xs)) ={map}
length (foldr (\x rec -> f x : rec) [] (x:xs)) ={foldr1}
length (f x : (foldr (\x rec -> f x : rec) [] xs)) ={L1}
1 + length (foldr (\x rec -> f x : rec) [] xs) ={h.i.}
1 + length xs ={L1}
length (x:xs) listo ?