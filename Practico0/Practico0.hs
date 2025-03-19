{-# OPTIONS_GHC -fno-warn-tabs #-}

module Practico0 where 

import LookupTable

type X = String
type Z = Int

data V = Conjunto [Z] 
    |   Booleano Bool

type Mem = LookupTable.Table X V

data Expr = Var X
    |   Asignacion X V
    |   Vacio 
    |   Unit Z
    |   Pertenencia Z Expr
    |   Union Expr Expr
    |   Interseccion Expr Expr
    |   Diferencia Expr Expr
    |   Inclusion Expr Expr

belongs :: Int -> [Int] -> Bool
belongs v [] = False
belongs v (x:xs) 
    | v == x = True 
    | otherwise = belongs v xs 

union :: [Int] -> [Int] -> [Int]
union [] l = l 
union (x:xs) l = x:(union xs l)

intersection :: [Int] -> [Int] -> [Int]
intersection [] l = []
intersection l [] = []
intersection (x:xs) l
    | belongs x l = x : (intersection xs l)
    | otherwise = intersection xs l 

difference :: [Int] -> [Int] -> [Int]
difference [] l = l
difference l [] = l
difference (x:xs) l
    | belongs x l = difference xs l
    | otherwise = x : (difference xs l)

included :: [Int] -> [Int] -> Bool
included [] l = True
included (x:xs) l = belongs x l && included xs l

type TwoListFunction = [Int] -> [Int] -> V
union_v l1 l2 = Conjunto (union l1 l2)
intersection_v l1 l2 = Conjunto (intersecition l1 l2)
union_v l1 l2 = Conjunto (union l1 l2)
union_v l1 l2 = Conjunto (union l1 l2)

get_func :: Expr -> Maybe (TwoListFunction, Expr, Expr)
get_func Union e1 e2 = Just (union, e1, e2) 
get_func Interseccion e1 e2 = Just (, e1, e2) 
get_func Diferencia e1 e2 = Just (union, e1, e2) 
get_func Inclusion e1 e2 = Just (union, e1, e2) 

eval :: Expr -> Mem -> (V, Mem)
eval (Var x) m = case (lkup x m) of
    Nothing -> error $ "Variable no se encontro ::" ++ x
    Just v -> (v, m)
eval (Asignacion x v) m = (v, upd (x, v) m)
eval Vacio m = (Booleano True, m) -- No se si esta bien
eval (Unit z) m = (Conjunto [z], m)
eval (Pertenencia z e) m = case eval e m of 
    (Conjunto l, m') -> (Booleano $ belongs z l, m')
    (Booleano _, _) -> error "Pertenencia no se puede evaluar con Booleano"
eval e m = 
    let (v1, m') = eval e1 m in
    let (v2, m'') = eval e2 m' in
    case (v1, v2) of 
    (Booleano _, _) -> error "Union no se puede evaluar con primer elemento Boolenao"
    (_, Booleano _) -> error "Union no se puede evaluar con segundo elemento Boolenao"
    (Conjunto l1, Conjunto l2) -> (Conjunto union l1 l2, m'')




