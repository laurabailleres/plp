ejercicio 3.vii

Probar que reverse = foldr (\x rec -> rec ++ (x:[])) []

reverse :: [a] -> [a]
foldr (\x rec -> rec ++ (x:[])) [] :: [a] -> [a]

foldr :: (a -> b -> b) -> b -> [a] -> b
(\x rec -> rec ++ (x:[])) :: a -> [a] -> [a]   
foldr (\x rec -> rec ++ (x:[])) :: [a] -> [a] -> [a]
foldr (\x rec -> rec ++ (x:[])) [] :: [a] -> [a]

Lo que queremos probar compara dos funciones
Para probar que son iguales, necesitamos analizarlas 
punto a punto (usar extensionalidad)

Por extensionalidad, para probar:
    reverse = foldr (\x rec -> rec ++ (x:[])) []
Alcanza ver que 
    ∀ xs :: [a]
    reverse xs = foldr (\x rec -> rec ++ (x:[])) [] xs

Por induccion en xs
Vamos a probar 
    P(xs): (reverse xs = foldr (\x rec -> rec ++ (x:[])) [] xs)

Caso base
P([]): (reverse [] = foldr (\x rec -> rec ++ (x:[])) [] [])

Lado izq
reverse [] ={R0} 
(foldl (flip (:)) []) [] ={Foldl0}
[] ={Foldl0}

Lado derecho
foldr (\x rec -> rec ++ (x:[])) [] [] ={Foldr0}
[] ={Foldr0}

ok

Paso inductivo
Supongo que vale P(xs), qvq vale P(x:xs)
    HI: P(xs) = (reverse xs = foldr (\x rec -> rec ++ (x:[])) [] xs)
    P(x:xs) = (reverse (x:xs) = foldr (\x rec -> rec ++ (x:[])) [] (x:xs))

Lado izq
reverse (x:xs) ={R0}
foldl (flip (:)) [] (x:xs) ={Foldl1}
foldl (flip (:)) (flip (:) [] x) xs ={FLIP}
foldl (flip (:)) ((:) x []) xs ={AAAAAAAAAAAAAAAAa}
foldl (flip (:)) (x:[]) xs ={...}
... vamos al otro lado a ver qué onda
    depsues de hacer el toro lado,vemos que onda
    Pesnando.....
    Para poder usar la equiv foldr-foldl necesito tener algo de estilo
    foldl (flip f) z (reverse lista) ={...}
    Nos faltaria hacer aparecer reverse
        xs ={id} id xs ={} (reverse . reverse) xs ={.} reverse (reverse xs)
    Ahi aprecio al revrse que queremos
foldl (flip (:)) (x:[]) xs ={Por Pesnamiento}
foldl (flip (:)) (x:[]) (reverse (reverse xs)) ={Por Pesnamiento}
foldl (flip (:)) (x:[]) (reverse (reverse xs)) ={Por equiv foldr-foldl}
foldl (:) (x:[]) (reverse xs) ={Por equiv foldr-foldl}
ponerAlFinal x (reverse xs) ={Def ponerAlFinal}
(reverse xs) ++ (x:[]) ={Por ej 3.VI}

Lado der
Sea f1 = (\y rec -> rec ++ (y:[]))

foldr f1 [] (x:xs) ={Foldr1}
f1 x (foldr f1 [] xs) ={def f1}
(\y rec -> rec ++ (y:[])) x (foldr f1 [] xs) ={beta (reemplazo x)}
(\rec -> rec ++ (x:[])) (foldr f1 [] xs) ={beta (reemplazo rec)}
(foldr f1 [] xs) ++ (x:[]) ={HI}
(reverse xs) ++ (x:[]) ={HI}

Def
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ ac [] = ac
foldl f ac (x:xs) = foldl f (f ac x) xs

Usamos
reverse . reverse = id
ponerAlFinal
Equiv foldr foldl

------------- EJEMPLO FOLDL -------------

sum = foldl (\rec x -> rec + x) 0 

f = (\rec x -> rec + x)
sum [1,2] ={sum}
foldl f 0 [1,2]
foldl f 0 1:(2:([])) ={Foldl1}
foldl f (f 0 1) (2:([])) ={Foldl1}
foldl f (f (f 0 1) 2) ([]) ={Foldl1}
(f (f 0 1) 2)
((+) ((+) 0 1) 2)
((+) (1) 2)
(3)
