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

--ejercicio 2
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

--ejercicio 3
--I
sumFold :: [Int] -> Int
sumFold = foldr (+) 0

elemFold :: Eq a => a -> [a] -> Bool
elemFold n = foldr (\x acc -> (x == n) || acc) False

concatFold :: [a] -> [a] -> [a]
concatFold (xs) (ys) = foldr (:) ys xs

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold f = foldr (\x rec -> if f x then x : rec else rec) []

mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr (\x rec -> f x : rec) []

--II
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun esMejor = foldr1 (\x rec -> if x `esMejor` rec then x else rec)

--III
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\acc a -> if null acc then [a] else acc ++ [a + last acc]) []

--IIII
sumaAlternada :: Num a => [a] -> a
sumaAlternada = foldr (-) 0

sumaAlternada2 :: Num a => [a] -> a --hace lo mismo que sumaAlternada
sumaAlternada2 = foldr1 (-)

--ejercicio 4
--I
ponerEnPosicion :: Int -> a -> [a] -> [a]
ponerEnPosicion n e xs = (take n xs) ++ [e] ++ (drop n xs)

--permutaciones :: [a] -> [[a]]

--II
partes :: [a] -> [[a]]
partes = foldr (\x rec -> rec ++ map (x:) rec) [[]]

--III
prefijosE :: [a] -> [[a]]
prefijosE [] = [[]]
prefijosE (x:xs) = [] : map (x:) (prefijosE xs)

prefijos :: [a] -> [[a]]
prefijos = foldr (\x rec -> [] : map (x:) rec) [[]]

--ejercicio 5
--elementosEnPosicionesPares NO usa recursión estructural porque usa 
--la lista (xs) para operar.
--en la recursión estructural solo podemos operar con x (o sea, un 
--elemento de la lista) y con la función recursiva en la lista.
--
--entrelazar usa recursión estructural ya que sí cumple con lo anterior.

--entrelazar :: [a] -> [a] -> [a]
--entrelazar (x:xs) (ys) = foldr (\ys rec -> x:(head ys):rec) (x:entrelazar xs [])

--ejercicio 6
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

--a
sacarUnaE :: Eq a => a -> [a] -> [a]
sacarUnaE _ [] = []
sacarUnaE n (x:xs) = if (x == n) then xs else x:(sacarUnaE n xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n = recr (\x xs rec -> if (x == n) then xs else x:rec) []

--b
--un esquema de recursión estructural no es el adecuado para resolver 
--este problema ya que necesito seguir usando la lista (xs) para operar.

--c
insertarOrdenadoE :: Ord a => a -> [a] -> [a]
insertarOrdenadoE n [] = [n]
insertarOrdenadoE n (x:xs) = if (n > x) then x:insertarOrdenadoE n xs else n:x:xs

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n = recr (\x xs rec -> if (n > x) then x:rec else n:x:xs) [n]

--ejercicio 7
-- ? ???? ? ? ??? ?

--ejercicio 8
--I
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat [] [] = []
sumaMat (x:xs) (y:ys) = zipWith (+) x y : sumaMat xs ys

--II


--ejercicio 9
foldNat :: (Int -> Int) -> Int -> Int -> Int
foldNat _ c 0 = c 
foldNat f c n = f (foldNat f c (n-1))

potenciaE :: Int -> Int -> Int
potenciaE _ 0 = 1
potenciaE n m = n * potenciaE n (m-1)

potencia :: Int -> Int -> Int
potencia n m = foldNat (*n) 1 m

--ejercicio 10
--I
genListaE :: a -> (a -> a) -> Integer -> [a]
genListaE i f 0 = []
genListaE i f n = (f i) : genListaE (f i) f (n-1)

--II
--desdeHasta :: (Int,Int) -> [Int]
--desdeHasta (x,y) = genLista ...

--ejercicio 11

data Polinomio a = X
    | Cte a
    | Suma (Polinomio a) (Polinomio a)
    | Prod (Polinomio a) (Polinomio a)

--ejemplos polinomios!!
--Cte 4
--Suma (Cte 1) (Cte 1)

foldPoli :: b -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli cCte cSuma cProd p = 
    case p of
        Cte c -> cCte
        Suma s1 s2 -> cSuma (rec s1) (rec s2)
        Prod p1 p2 -> cProd (rec p1) (rec p2)
    where
        rec = foldPoli cCte cSuma cProd

evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPoli n (\s1 s2 -> s1 + s2) (\p1 p2 -> p1 * p2)

--ejercicio 12
data AB a = Nil | Bin (AB a) a (AB a)

--ejemplos de árboles
--Bin (Nil) 0 (Bin (Nil) 0 (Nil))
----altura = 2
----nodos = 2

--I 
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin t = 
    case t of
        Nil -> cNil
        Bin i x d -> cBin (rec i) x (rec d)
    where
        rec = foldAB cNil cBin

recAB :: b -> (b -> AB a -> a -> b -> AB a -> b) -> AB a -> b
recAB cNil cBin t =
    case t of
        Nil -> cNil
        Bin i x d -> cBin (rec i) i x (rec d) d
    where
        rec = recAB cNil cBin

--II 
esNil :: AB a -> Bool
esNil t = 
    case t of
        Nil -> True
        _ -> False

altura :: AB a -> Int
altura = foldAB 0 (\ri _ rd -> 1 + max i d)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\ri _ rd -> 1 + ri + rd)

{-  cantNodos Nil = 0

cantNodos Nil ={cN}
foldAB 0 (\ri _ rd -> 1 + ri + rd) Nil ={foldAB}
case Nil of Nil -> 0; Bin i x d -> (\ri _ rd -> 1 + ri + rd) (rec i) x (rec d) ={caseNil}
0   -}

---III
mejorSegún :: (a -> a -> Bool) -> AB a -> a
mejorSegun f = recAB (error "turquia estado genocida") (\ri i x rd d -> aux f (aux f x ri i) rd d)

aux :: (a -> a -> Bool) -> a -> a -> AB a -> a
aux f x rt t =
    case t of
        Nil -> x
        _ -> if f x rt then x else rt

--ejercicio 13

--ejercicio 14

data AIH a = Hoja a | Bin (AIH a) (AIH a)

---a
foldAIH :: b -> (b -> b -> b) -> AIH a -> b
foldAIH cHoja cBin t =
    case t of
        Hoja a -> cHoja
        Bin i d -> cBin (rec i) (rec d)
    where
        rec = foldAIH cHoja cBin 

---b
altura :: AIH a -> Int
altura = foldAIH 1 (\ri rd -> 1 + max ri rd)

tamaño :: AIH a -> Int
tamaño = foldAIH 1 (\ri rd -> ri + rd)

--ejercicio 15
--data RoseTree a = 
