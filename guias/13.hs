data AB a = Nil | Bin (AB a) a (AB a)

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

--ramas

--cantHojas :: AB a -> Int
