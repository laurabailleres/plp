--I.
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

--II.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y)= f x y

--III. ¿se podría definir una función curryN, que tome una función de 
--un número arbitrario de argumentos y devuelva su versión currificada?
--sugerencia: pensar cuál sería el tipo de la función.
---no..