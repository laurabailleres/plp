Si no me acuerdo como llegar al tipo escribo algo como un "sistema de ecuaciones" para los tipos de cada variable
x :: a1
g :: b1 -> b2
f :: c1 -> c2

Escribo las restricciones (qué debe cumplir cada variable de tipo)
a1 = b1
b2 = c1

Resuelvo (reemplazo)
x :: a
g :: a -> b
f :: b -> c

iv. ∀ f::a->b . ∀ g::b->c . ∀ h::c->d . 
    ((h . g) . f) = (h . (g . f))
    
    Veo el tipo de la expresion que comparo
    ((h . g) . f) :: a -> d
    Como es una función, necesito usar

Extensionalidad (funcional)
    Para probar que vale ∀... 
        ((h . g) . f) = (h . (g . f))
    Alcanza con ver que  ∀... ∀ x::a. 
        ((h . g) . f) x = (h . (g . f)) x

Sean h, g, f, x (cada una del tipo correspondiente)
((h . g) . f) x = (h . (g . f)) x

Partiendo del lado izquierdo
((h . g) . f) x ={(.)}
(h . g) (f x) ={(.)}
h (g (f x))

Partiendo del lado derecho
(h . (g . f)) x ={(.)}
(h (g . f)) x ={(.)}
h (g (f x))

Composición: 
(.) :: (b -> c) -> (a -> b) -> a -> ?
(.) f g x = f (g x)
(f . g) x = f (g x)