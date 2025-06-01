--ejercicio 1
--idea general: para el ítem I hago :t nombredelafuncion en la consola. 
--para el item II me fijo si es válido hacer :t nombreDeLaFunción 
--primerParámetro o si me devuelve error. si funciona es que está
--currificada, sino la tengo que currificar :)
--asumo que todos los números son de tipo Float

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
    --I tipo: Float -> Float -> Float
    --II ya está currificada

predecesor = substract 1
    --I tipo: Float -> Float
    --II ya está currificada

evaluarEnCero = \f -> f 0
    --I tipo: (Float -> t) -> t // es decir: recibe como parámetro 
    ---una función de tipo Float -> t y devuelve t. 
    --II ya está currificada

dosVeces = \f -> f . f
    --I tipo: (a -> a) -> a -> a // recibe como parámetros una función 
    ---de a en a y un a. le aplica al a dos veces la función.
    --II ya está currificada.

flipAll = map flip
    --I tipo: [a -> b -> c] -> [b -> a -> c]
    --II ya está currificada.

flipRaro = flip flip
    --I tipo: b -> (a -> b -> c) -> a -> c
    --II ya está currificada.
