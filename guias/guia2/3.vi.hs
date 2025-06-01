∀ xs::[a] . ∀ x::a . ponerAlFinal x xs = xs ++ (x:[])

     ponerAlFinal :: a -> [a] -> [a]
{P0} ponerAlFinal x = foldr (:) (x:[])

      (++) :: [a] -> [a] -> [a]
{++0} [] ++ ys = ys
{++1} (x:xs) ++ ys = x : (xs ++ ys)

quiero probar que dos listas son iguales!!
uso inducción estructural en listas

caso base P([])

ponerAlFinal x [] = [] ++ (x:[]) ={P0}
foldr (:) (x:[]) [] = [] ++ (x:[]) ={foldr0}
x:[] = [] ++ (x:[]) ={++0}
x:[] = x:[] listo

paso inductivo P(ys) ⇒ P(y:ys)
h.i. ponerAlFinal x ys = ys ++ (x:[])
q.v.q. ponerAlFinal x (y:ys) = (y:ys) ++ (x:[])

ponerAlFinal x (y:ys) ={P1}
foldr (:) (x:[]) (y:ys) ={foldr1}
(:) y (foldr (:) (x:[]) ys) ={cambio de notación}
y : (foldr (:) (x:[]) ys) ={P0}
y : (ponerAlFinal x ys) ={h.i.}
y : (ys ++ x:[]) ={++1}
(y:ys) ++ (x:[]) :D: