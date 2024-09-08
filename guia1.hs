--ejercicio 1
--idea general: para el ítem I hago :t nombredelafuncion en la consola. para el item II me fijo si 
--es válido hacer :t nombredelafuncion primerparametro o si me devuelve error. si funciona es que está
--currificada, sino la tengo que currificar :)

max2 (x, y) | x >= y = x
            | otherwise = y
    --I tipo: (Float, Float) -> Float
    --II no está currificada
max2C :: Float -> Float -> Float
max2C x y = max2 (x,y)
    --nuevo tipo: Float -> Float -> Float

normaVectorial (x, y) = sqrt (x^2 + y^2)
    --I tipo: (Float, Float) -> Float
    --II no está currificada
normaVectorialC :: Float -> Float -> Float
normaVectorialC x y = normaVectorial (x,y)
    --nuevo tipo: Float -> Float -> Float

substract = flip (-)
    --I tipo: Num -> Num
    --II ya está currificada

predecesor = substract 1
    --I tipo: Int -> Int
    --II ya está currificada

evaluarEnCero = \f -> f 0
    --I tipo: (Integer -> t) -> t // es decir: recibe como parámetro una función de tipo Int -> t 
    --y devuelve t. 
    --II ya está currificada

dosVeces = \f -> f . f
    --I tipo: (a -> a) -> a -> a // recibe como parámetros una función y un a. le aplica a a dos
    --veces la función.
    --II ya está currificada.

flipAll = map flip
    --I tipo: [a -> b -> c] -> [b -> a -> c]
    --II ya está currificada.

flipRaro = flip flip
    --I tipo: b -> (a -> b -> c) -> a -> c
    --II 

--ejercicio 2
--I.
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

--II.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y)= f x y

--III. ¾Se podría definir una función curryN, que tome una función de un número arbitrario de argumentos y
--devuelva su versión currificada?
--Sugerencia: pensar cuál sería el tipo de la función.

--ejercicio 3
--I
sumFold :: [Int] -> Int
sumFold = foldr (+) 0

elemFold :: Eq a => a -> [a] -> Bool
elemFold n = foldr (\x acc -> (x == n) || acc) False

concatFold :: [a] -> [a] -> [a]
concatFold (x:xs) (ys) = foldr (:) (ys) (x:xs)

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold f = foldr (\x rec -> if f x then x:rec else rec) []


mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr (\x rec -> f x : rec) []

--II
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

--III
sumaAlternada :: Num a => [a] -> a
sumaAlternada = foldr (-) 0
