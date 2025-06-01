--elementosEnPosicionesPares NO usa recursión estructural porque usa 
--la lista (xs) para operar.
--en la recursión estructural solo podemos operar con x (o sea, un 
--elemento de la lista) y con la función recursiva en la lista.
--
--entrelazar usa recursión estructural ya que sí cumple con lo anterior.

--entrelazar :: [a] -> [a] -> [a]
--entrelazar (x:xs) (ys) = foldr (\ys rec -> x:(head ys):rec) (x:entrelazar xs [])
