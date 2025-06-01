     zip :: [a] -> [b] -> [(a,b)]
{Z0} zip = foldr (\x rec ys ->
        if null ys
          then []
          else (x, head ys) : rec (tail ys))
    (const [])

      zip’ :: [a] -> [b] -> [(a,b)]
{Z’0} zip’ [] ys = []
{Z’1} zip’ (x:xs) ys = if null ys 
                         then [] 
                         else (x, head ys):zip’ xs (tail ys)

quiero demostrar zip = zip’

     zip = zip’ 

     zip  :: [a] -> [b] -> [(a,b)]
     zip' :: [a] -> [b] -> [(a,b)]

estoy comparando funciones 
por extensionalidad funcional, 
     zip xs = zip’ xs

     zip xs  :: [b] -> [(a,b)]
     zip' xs :: [b] -> [(a,b)]
    
sigo comparando funciones
por extensionalidad funcional,
     zip xs ys = zip’ xs ys

     zip xs ys  :: [(a,b)]
     zip’ xs ys :: [(a,b)]

ahora estoy comparando dos listas 

entonces lo que quiero demostrar es:
∀ xs::[a] . ∀ ys::[a] . zip xs ys = zip’ xs ys

hago inducción estructural sobre una de las listas (xs)

entonces tengo
∀ xs::[a] . P(xs)

me perdí