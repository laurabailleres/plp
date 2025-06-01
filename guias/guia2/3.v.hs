∀ xs::[a] . ∀ p::a->Bool . ∀ e::a . ((elem e (filter p xs)) ⇒ (elem e xs)) (asumiendo Eq a)

     elem :: a -> [a] -> Bool
{E0} elem [] = False
{E1} elem n (x:xs) = (n == x) || elem n xs

       filter :: (a -> Bool) -> [a] -> [a]
{F0} = filter f [] = []
{F1} = filter f (x:xs) = if f x then x:(filter f xs) else filter f xs


agrego formalidad. ....

lo que quiero demostrar es:
∀ xs::[a] . ∀ p::a->Bool . ∀ e::a . (elem e (filter p xs)) = True ⇒ (elem e xs) = True

defino P(xs)
P(xs) = ∀ p::a->Bool . ∀ e::a . (elem e (filter p xs)) = True ⇒ (elem e xs) = True

quiero probar que ∀ xs::[a] . P(xs)

hago inducción estructural en listas

caso base P([])
P([]) = ∀ p::a->Bool . ∀ e::a . (elem e (filter p [])) = True ⇒ (elem e []) = True

elem e (filter p []) ={F0}
elem e [] ={E0}
False

entonces ((elem e (filter p [])) = True) es falso porque False = True es falso
en las implicaciones Falso ⇒ cualquier cosa !!!! entonces listo

paso inductivo ∀ x::a . ∀ xs::[a] . P(xs) ⇒ P(x:xs)
P(x:xs) = ∀ p::a->Bool . ∀ e::a . (elem e (filter p (x:xs))) = True ⇒ (elem e (x:xs)) = True

h.i.   ∀ p::a->Bool . ∀ e::a . (elem e (filter p (xs))) = True ⇒ (elem e (xs)) = True
q.v.q. ∀ p::a->Bool . ∀ e::a . (elem e (filter p (x:xs))) = True ⇒ (elem e (x:xs)) = True

elem e (filter p (x:xs)) ={F1}
elem e (if p x then x:(filter p xs) else filter p xs)

uso lema de generación de bool. p x puede ser True o False

caso p x = False
elem e (if p x then x:(filter p xs) else filter p xs) ={p x = False}
elem e (if False then x:(filter p xs) else filter p xs) ={False}
elem e (filter p xs) ⇒{h.i.} elem e xs 

caso p x = True
elem e (if p x then x:(filter p xs) else filter p xs) ={p x = True}
elem e (if True then x:(filter p xs) else filter p xs) ={True}
elem e (x:(filter p xs)) ={E1}
e == x || elem e (filter p xs)













como quiero probar una implicación, niego el lado derecho de la flecha y veo que esto 
solo puede suceder si lo que esta a la izquierda es falso (contrarrecíproco para los densos.....)

      entonces tengo que 
{neg} ¬(elem e xs)

y quiero ver que asumiendo {neg},
    elem e (filter p xs)
es falso 

o sea esto: ¬(elem e xs) => ¬(elem e (filter p xs))
asumiendo que ¬(elem e xs) es verdadero 

divido en casos: [] y (x:xs)

caso []
¬(elem e []) => ¬(elem e (filter p []))
asumiendo que ¬(elem e []) es verdadero

elem e (filter p []) ={F0}
elem e [] ={E0}
False

okey

caso (x:xs)
¬(elem e (x:xs)) => ¬(elem e (filter p (x:xs)))
asumiendo que ¬(elem e (x:xs)) es verdadero

elem e (filter p (x:xs)) ={F1}
elem e (if p x then x:(filter p xs) else (filter p xs))

por extensionalidad de bool, (p x) puede ser True o False
caso True
elem e (x:filter p xs) ={E1}
(x == e) || elem e (filter p xs) ={neg}
(x == e) || False 
como yo asumí que ¬(elem e (x:xs)), por {E1} x != e.
entonces
False || False =
False


caso False
elem e (filter p xs) ={neg}
False