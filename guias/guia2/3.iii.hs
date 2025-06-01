iii. ∀ xs::[a] . ∀ x::a . append [x] xs = x:xs

     append :: [a] -> [a] -> [a]
{A0} append xs ys = foldr (:) ys xs

append [x] xs :: [a]
x:xs :: [a] 

quiero comparar dos listas :D

uso lema de generación de listas. pruebo que vale para [] y para (x:xs) 
(o sea los dos constructores del tipo)

P(xs): append [x] xs = x:xs

P([]): append [x] [] = x:[]

lado izquierdo
append [x] [] ={A0}
foldr (:) [] [x] ={foldr0}
[x] 

lado derecho
x:[] = [x] ????????????!!
listo 
?

P(y:ys)

q.v.q. append [x] (y:ys) = x:(y:ys)

append [x] (y:ys) ={A0}
foldr (:) (y:ys) [x] ={foldr1}
(:) x (foldr (:) (y:ys) []) ={foldr0}
(:) x (y:ys) llegué a lo que quería ??