data AB a = Nil | Bin (AB a) a (AB a)

--ejemplos de árboles
--Bin (Nil) 0 (Bin (Nil) 0 (Nil))
--Bin (Bin (Nil) 2 (Nil)) 1 (Bin (Nil) 2 (Nil))
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
altura = foldAB 0 (\ri _ rd -> 1 + max ri rd)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\ri _ rd -> 1 + ri + rd)

{-  cantNodos Nil = 0

cantNodos Nil ={cN}
foldAB 0 (\ri _ rd -> 1 + ri + rd) Nil ={foldAB}
case Nil of Nil -> 0; Bin i x d -> (\ri _ rd -> 1 + ri + rd) (rec i) x (rec d) ={caseNil}
0   -}

---III
mejorSegun :: (a -> a -> Bool) -> AB a -> a
mejorSegun f = recAB (error "Nil") (\ri i x rd d -> aux f (aux f x ri i) rd d)

aux :: (a -> a -> Bool) -> a -> a -> AB a -> a
aux f x rt t =
    case t of
        Nil -> x
        _ -> if f x rt then x else rt

---IV
raiz :: AB a -> a
raiz (Bin i x d) = x

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\ri i x rd d -> ((raiz i) <= x) && (x < (raiz d)) && (ri && rd))

---V
