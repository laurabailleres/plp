reverse . reverse = id

    reverse :: [a] -> [a]
{R} foldr (\x rec -> rec ++ (x:[])) []

    id :: a -> a
{I} id x = x

    (.) :: (b -> c) -> (a -> b) -> a -> c
{.} (.) f g x = f (g x)

como quiero probar que dos funciones son iguales tengo que usar extensionalidad funcional 

basta con ver que ∀ xs::[a]
reverse . reverse xs = id xs

entonces lo que quiero demostrar es:
∀ xs::[a] . reverse . reverse xs = id xs

defino P(xs)
P(xs) = reverse . reverse xs = id xs

quiero probar que ∀ xs::[a] . P(xs)

hago inducción estructural en listas

caso base P([])
P([]) = reverse . reverse [] = id []

reverse . reverse [] ={.}
reverse (reverse []) ={R}
reverse (foldr (\x rec -> rec ++ (x:[])) [] []) ={foldr0}
reverse [] ={R}
foldr (\x rec -> rec ++ (x:[])) [] [] =={foldr0}
[] ={I}
id []

ok....

paso inductivo ∀ x::a . ∀ xs::[a] . P(xs) ⇒ P(x:xs)
P(x:xs) = reverse . reverse (x:xs) = id (x:xs)

h.i.   reverse . reverse (xs) = id (xs)
q.v.q. reverse . reverse (x:xs) = id (x:xs)

reverse . reverse (x:xs) ={.}
reverse (reverse (x:xs)) ={R}
reverse (foldr (\x rec -> rec ++ (x:[])) [] (x:xs)) =