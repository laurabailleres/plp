data HashSet a = Hash (a -> Int) (Int -> [a])

---I
vacio :: (a -> Int) -> HashSet a
vacio f = Hash f (\_ -> [])

---II
--pertenece :: Eq a => a -> HashSet a -> Bool

---III
--agregar :: Eq a => a -> HashSet a -> HashSet a
