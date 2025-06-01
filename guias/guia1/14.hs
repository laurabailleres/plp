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
