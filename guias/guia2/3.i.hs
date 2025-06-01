i. ∀ xs::[a] . length (duplicar xs) = 2 * length xs

     length :: [a] -> Int
{L0} length [] = 0
{L1} length (x:xs) = 1 + length xs

     duplicar :: [a] -> [a]
{D0} duplicar [] = []
{D1} duplicar (x:xs) = x : x : duplicar xs

hago inducción estructural en listas

caso base P([])
length (duplicar []) = 2 * length [] ={D0}
length [] = 2 * length [] ={L0}
0 = 2 * length [] ={L0}
0 = 2 * 0 ={aritmética ??}
0 = 0 so true bestie 

paso inductivo  P(xs) => P(x:xs)

h.i. length (duplicar xs) = 2 * length xs
q.v.q. length (duplicar (x:xs)) = 2 * length (x:xs)

length (duplicar (x:xs)) ={D1} 
length (x:x:duplicar xs) ={L1}
1 + length (x:duplicar xs) ={L1}
2 + length (duplicar xs) ={h.i.}
2 + 2 * length xs ={saco factor común}
2 * (length xs + 1) ={L1}
2 * (length x:xs) 

<3