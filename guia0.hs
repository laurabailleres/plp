--ejercicio 1
--null: función que toma una lista y devuelve si es nula
--input:    null []    /   output:   True
--input:    null [1]    /   output:   False

--head: función que toma una lista y devuelve el primer elemento
--input:    head [1, 2, 3]    /   output:   1

--tail: función que toma una lista y la devuelve sin el primer elemento
--input:    [1]    /   output: []
--input:    [1, 2, 3]    /   output: [2, 3]

--init: función que toma una lista y la devuelve sin el último elemento
--input:    [1]    / output:    []
--input:    [1, 2, 3]   /   output:    [1, 2]

--last: función que toma una lista y devuelve el último elemento
--input:    last [1, 2, 3]    /   output:   3

--take: función que toma una lista xs y un entero n y devuelve otra lista con los n-primeros elementos de xs
--input:    take 0 [1, 2, 3]    /   output:    []
--input:    take 2 [1, 2, 3]    /   output:    [1, 2]

--drop: función que toma una lista xs y un entero n y devuelve otra lista sin los n-primeros elementos de xs
--input:    drop 0 [1, 2, 3]    /   output:    [1, 2, 3]
--input:    drop 2 [1, 2, 3]    /   output:    [3]
--input:    drop 3 [1, 2, 3]    /   output:    []

--(++): función que toma dos listas y las concatena
--input:    [1] ++ [2]    /   output:   [1, 2]
--input:    [1, 2] ++ [2, 3]    /   output:    [1, 2, 2, 3]

--concat: función que toma una lista de listas y las concatena
--input:    concat [[1], [2], [3]]    /   output:    [1, 2, 3]

--reverse: función que toma una lista y la devuelve invertida
--input:    reverse [1, 2, 3]   /   output:    [3, 2, 1]

--elem: función que toma un elemento n y una lista xs y devuelve si n está contenido en xs
--input:    elem 1 [1, 2, 3]    /   output:    True
--input:    elem 5 [1, 2, 3]    /   output:    False   



--ejercicio 2
---a
valorAbsoluto :: Float -> Float
valorAbsoluto n | n > 0 = n
                | n < 0 = (-1) * n

---b
bisiesto :: Integer -> Bool
bisiesto n = (mod n 4 == 0) && (mod n 100 /= 0)

---c
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

---d




--ejercicio 3
---a
inverso :: Float -> Maybe Float
inverso n | n == 0 = Nothing
          | otherwise = Just (1/n)

---b
aEntero :: Either Integer Bool -> Integer
aEntero n = case n of
    (Left n) -> n
    (Right n) -> if n then 1 else 0



--ejercicio 4
---a
eliminarChar :: Char -> String -> String
eliminarChar _ [] = [] 
eliminarChar n (x:xs) | n == x = eliminarChar n xs
                      | otherwise = x : eliminarChar n xs

limpiar :: String -> String -> String
limpiar [] str = str
limpiar (x:xs) str = limpiar xs (eliminarChar x str)


---b
sumaElementos :: [Float] -> Float
sumaElementos [] = 0
sumaElementos (x:xs) = x + sumaElementos xs

longitud :: [Float] -> Float
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

promedio :: [Float] -> Float
promedio xs = (sumaElementos xs) / (longitud xs)

difPromedio :: [Float] -> [Float]
difPromedio xs = map (\x -> x - promedio xs) xs


---c
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) | y /= x = True
                      | otherwise = todosIguales xs




--ejercicio 5
data AB a = Nil | Bin (AB a) a (AB a)

---a
vacioAB :: AB a -> Bool
vacioAB n = case n of
    (Nil) -> True

---b
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq valor der) = Bin (negacionAB izq) (not valor) (negacionAB der)
