--POR AHORA EXACTAS FUNCIONA, PERO LOS PROBLEMAS SIGUEN!
--GRILLA SALARIAL DOCENTE: https://aduba.org.ar/wp-content/uploads/2024/07/Instructivo-Liquidacion-Salarios-JULIO-2024.pdf






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

