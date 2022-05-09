subst :: Eq t => t -> t -> [t] -> [t]
subst _ _ [] =[]
subst x y (z:zs)
    | z==x = (y:subst x y zs)
    | otherwise = x:subst x y zs

subst :: Eq t => t -> t -> [t] -> [t]
subst _ _ [] =[]
subst y z  (x:xs) 
    | x==y = (z:subst y z xs)
    |otherwise = x:subst y z xs