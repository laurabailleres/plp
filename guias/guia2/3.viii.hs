     ponerAlFinal :: a -> [a] -> [a]
{P0} ponerAlFinal x = foldr (:) (x:[])

     reverse :: [a] -> [a]
{R0} reverse = foldr (\x rec -> rec ++ (x:[])) []

    head :: [a] -> a
{H} head (x:xs) = x

∀ xs::[a] . ∀ x::a . head (reverse (ponerAlFinal x xs)) = x

defino P(xs)
∀ x::a . head (reverse (ponerAlFinal x xs)) = x

quiero probar que ∀ xs::[a] . P(xs)

hago inducción estructural en listas

caso base P([])
P([]) = ∀ x::a . head (reverse (ponerAlFinal x xs)) = x

head (reverse (ponerAlFinal x [])) = x ={P0}
head (reverse (foldr (:) (x:[])) []) = x ={foldr0}
head (reverse (x:[])) = x ={R1}
head (foldr (\x rec -> rec ++ (x:[])) [] (x:[])) = x ={foldr1}
head (x:(foldr (\x rec -> rec ++ (x:[])) [] [])) = x ={foldr0}
head (x:[]) = x ={H}
x = x listo

paso inductivo ∀ y::a . ∀ ys::[a] . P(ys) ⇒ P(y:ys)
P(y:ys) = ∀ x::a . head (reverse (ponerAlFinal x (y:ys))) = x

h.i.   ∀ x::a . head (reverse (ponerAlFinal x ys)) = x
q.v.q. ∀ x::a . head (reverse (ponerAlFinal x (y:ys))) = x

head (reverse (ponerAlFinal x (y:ys))) = x ={P0}
head (reverse (foldr (:) (x:[]) x (y:ys))) = x ={foldr1}
head (reverse (y:(foldr (:) (x:[]) x ys))) = x ={reescribo}
head (reverse (y:(ponerAlFinal x ys))) = x ={h.i.} ???