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