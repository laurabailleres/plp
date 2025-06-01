---I
data RoseTree a = Rose a [RoseTree a]

---II
foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose a xs) = f a (map (foldRose f) xs)

---III
--hojas :: RoseTree a -> [RoseTree a]

--distancias

--altura :: RoseTree a -> Int