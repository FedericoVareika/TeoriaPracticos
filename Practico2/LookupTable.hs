{-# OPTIONS_GHC -fno-warn-tabs #-}

module LookupTable where 

type Tabla a b = [(a, b)] 

create :: Tabla a b 
create = []

update :: Eq a => Tabla a b -> [(a, b)]  -> Tabla a b 
-- update = foldr upd 
update m (t : ts) = update (upd t m) ts -- esta aca el error
update m [] = m

upd :: Eq a => (a, b) -> Tabla a b -> Tabla a b 
upd t [] = [t]
upd t@(x, y) (t'@(x', y') : ts) 
    | x == x' = t : ts   
    | otherwise = t' : upd t ts

lkup :: Eq a => a -> Tabla a b -> Maybe b
lkup k [] = Nothing
lkup k (x : xs) 
    | k == fst x = Just $ snd x
    | otherwise = lkup k xs 

bajas :: Eq a => Tabla a b -> [a] -> Tabla a b
bajas = foldr del 

del :: Eq a => a -> Tabla a b -> Tabla a b 
del k [] = []
del k (x : xs) 
    | k == fst x = xs
    | otherwise = x : del k xs 


