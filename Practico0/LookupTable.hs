{-# OPTIONS_GHC -fno-warn-tabs #-}

module LookupTable where 

type Table a b = [(a, b)]

create :: Table a b 
create = []

upd :: Eq a => (a, b) -> Table a b -> Table a b 
upd x [] = [x]
upd x (x' : xs) 
    | fst x == fst x' = x : xs   
    | otherwise = x' : upd x xs

lkup :: Eq a => a -> Table a b -> Maybe b
lkup k [] = Nothing
lkup k (x : xs) 
    | k == fst x = Just $ snd x
    | otherwise = lkup k xs 

del :: Eq a => a -> Table a b -> Table a b 
del k [] = []
del k (x : xs) 
    | k == fst x = xs
    | otherwise = x : del k xs 


