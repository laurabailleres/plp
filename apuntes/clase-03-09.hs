--en general
--recursión estructural: primero lo pienso en recursión explíita y después 
--la paso a estructural.

--estructural -> x (g xs)
--primitiva -> x (g xs) xs
--global -> x xs g



--elem: nos dice si un elemento está en una lista

elem2 :: Eq q => a -> [a] -> Bool
elem2 e [] = False
elem2 e (x:xs) = x == e || elem2 e xs

--recursión estructural

elem3 :: (Foldable t, Eq a) => a -> t a -> Bool
elem3 e = foldr (\x rec -> x == e || rec) False



--sumaAlternada: realiza la suma alternada de los elementos de una lista.
--[1, 2, 3, 4] --> + 1 - 2 + 3 - 4

sumaAlternada [] = 0
sumaAlternada (x:xs) = x - (sumaAlternada xs)

sumaAlternadaF :: Num a => [a] -> a
sumaAlternadaF= foldr (\x rec -> x - rec) 0



--take usando foldr

takev2 :: Int -> [a] -> [a]
takev2 n [] = []
takev2 n (x:xs) = if n == 0 then [] else x : takev2 (n-1) xs

takev4 :: [a] -> (Int ->[a])
take4 [] = \n -> [] -- const []
take4 (x:xs) = \n -> if n == 0 then [] else x : take4 xs (n-1)

takev5 :: [a] -> Int -> [a]
takev5 = foldr (\x rec -> \n -> if n == 0 then [] else x : rec (n-1))
               (\n -> [])

takev6 :: Int -> [a] -> [a]
take6 = flip take5



--sacarUna: elimina la primera aparición de un elemento en una lista.

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e [] = []
sacarUna e (x:xs) = if e == x then xs else x : sacarUna e xs

sacarUnav2 :: Eq a => a -> [a] -> [a]
sacarUnav2 e = recr (\x xs rec -> if e == x then xs else x : rec)
                    []



--pares :: [(Int, Int)] que contenga todos los pares de los números pares

-- [ f x | x <- xs, p x] -- map y filter // definición por comprensión 
--ej: [x | x <- [True, False, False, True], x]
--[True, True]



-----------------------------------

--folds sobre estructuras nuevas--
 
-----------------------------------

--sea el siguiente tipo:
--data AEB a = Hoja a | Bin (AEB a) a (AEB a)"

data AEB a = Hoja a | Bin (AEB a) a (AEB a)

foldAEB :: () -> AEB a -> b
foldAEB fBin fHoja t = case t of
                            Hoja h -> fHoja h
                            Bin i r d -> fBin (rec i) (rec d)
    where rec = foldAEB fBin fHoja

hojas :: AEB a -> [a]
hojas = foldAEB (\r ri rd -> ri ++ rd) (\h -> [h])

--ejemplo:
miArbol = Bin (Hoja 3) 5 (Bin (Hoja 7) 8 (Hoja 1))



data Polinomio a = X
                | Cte a
                | Suma (Polinomio a) (Polinomio a)
                | Prod (Polinomio a) (Polinomio a)

evaluar :: Num => a -> a -> Polinomio a -> acc
evaluar x pol = case pol of 
                | X -> x
                | Cte c -> c
                | Suma p q -> evaluar x p + evaluar x q
                | Prod p q -> evaluar x p * evaluar x q

foldPoli :: (b) -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli fX fCte fSuma fProd pol = case pol of
                                    | X -> X
                                    | Cte c -> fCte c
                                    | Suma p q -> fSuma (rec p) (rec q)
                                    | Prod p q -> fProd (rec p) (rec q)
    where rec = foldPoli fX fCte dSuma fProd

evaluarv2 :: Num a => a -> Polinomio a -> a
evaluarv2 x = foldPoli x id (+) (*)



data RoseTree a = Rose a [RoseTree a]

Rose 1 [Rose 2 [Rose 3 [], Rose 4 [Rose 5 [], Rose 6 [], Rose 7[]]]]

foldRT :: (a -> [b]-> b) -> RoseTree a -> b
foldRT fRose (Rose n hijos) = fRose n (map rec hijos)
    where rec = foldRT fRose

recRT :: (a -> [RoseTree a] -> [b] -> b) -> RoseTree a -> b

cantNodos :: RoseTree a -> Int
cantNodos = foldRT (\n rhijos -> 1 + sum rhijos)

hojas :: RoseTree a -> [a]
hojas =foldRT (\n rec -> if null rec then [n] else concat rec)

ramas :: RoseTree a -> [[a]]
ramas = foldRT (\n rec -> if null rec then [[n]] else map (n:) (concat rec))



---------------------------------------

--funciones como estructuras de datos--
 
---------------------------------------

type Conj a = (a -> Bool)

vacio :: Conj a 
vacio = const False

agregar :: Eq a => a -> Conj a -> Conj a
agregar e c = \e2 -> \e2 == e || c

interseccion :: Conj a -> Conj a -> Conj a
interseccion c1 c2 = \e -> c1 e && c2 e

union :: Conj a -> Conj a -> Conj a
union c1 c2 = \e -> c1 e || c2 e

diferencia :: Conj a -> Conj a -> Conj acc
diferencia c1 c2 = \e -> c1 e && not (c2 e)

complemento :: Conj a -> Conj a
complemento c1 = not c1
