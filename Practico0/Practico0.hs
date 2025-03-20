{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Practico0 where

import LookupTable
import Tests.HUnit

type X = String 

type Z = Int

data V
  = Conjunto [Z]
  | Booleano Bool
  deriving Show

type Mem = LookupTable.Table X V

data Expr
  = Var X
  | Asignacion X Expr
  | Vacio
  | Unit Z
  | Pertenencia Z Expr
  | Union Expr Expr
  | Interseccion Expr Expr
  | Diferencia Expr Expr
  | Inclusion Expr Expr
  deriving Show

belongs :: Int -> [Int] -> Bool
belongs v [] = False
belongs v (x : xs)
  | v == x = True
  | otherwise = belongs v xs

union :: [Int] -> [Int] -> [Int]
union [] l = l
union (x : xs) l
    | not $ belongs x l = x : xs `union` l
    | otherwise = xs `union` l

intersection :: [Int] -> [Int] -> [Int]
intersection [] l = []
intersection l [] = []
intersection (x : xs) l
  | belongs x l = x : intersection xs l
  | otherwise = intersection xs l

difference :: [Int] -> [Int] -> [Int]
difference [] l = l
difference l [] = l
difference (x : xs) l
  | belongs x l = difference xs l
  | otherwise = x : difference xs l

included :: [Int] -> [Int] -> Bool
included l1 l2 = foldr (\x -> (&&) $ belongs x l2) True l1

type TwoListFunction = [Int] -> [Int] -> V

union_v l1 l2 = Conjunto (l1 `union` l2)

intersection_v l1 l2 = Conjunto (intersection l1 l2)

difference_v l1 l2 = Conjunto (difference l1 l2)

included_v l1 l2 = Booleano (included l1 l2)

get_func :: Expr -> Maybe (TwoListFunction, Expr, Expr)
get_func (Union e1 e2) = Just (union_v, e1, e2)
get_func (Interseccion e1 e2) = Just (intersection_v, e1, e2)
get_func (Diferencia e1 e2) = Just (difference_v, e1, e2)
get_func (Inclusion e1 e2) = Just (included_v, e1, e2)
get_func _ = Nothing

eval :: Expr -> Mem -> (V, Mem)
eval (Var x) m = case lkup x m of
  Nothing -> error $ "Variable no se encontro ::" ++ x
  Just v -> (v, m)
eval (Asignacion x e) m = let (v, m') = eval e m in 
    (v, upd (x, v) m')
eval Vacio m = (Conjunto [], m)
eval (Unit z) m = (Conjunto [z], m)
eval (Pertenencia z e) m = case eval e m of
  (Conjunto l, m') -> (Booleano $ belongs z l, m')
  (Booleano _, _) -> error "Pertenencia no se puede evaluar con Booleano"

eval e m =
  let (v1, m') = eval e1 m
   in let (v2, m'') = eval e2 m'
       in case (v1, v2) of
            (Booleano _, _) -> error "Union no se puede evaluar con primer elemento Booleano"
            (_, Booleano _) -> error "Union no se puede evaluar con segundo elemento Booleano"
            (Conjunto l1, Conjunto l2) -> (func l1 l2, m'')
  where
    (func, e1, e2) = case get_func e of 
        Nothing -> error "Unexpected expression for get_func"
        Just (func, e1, e2) -> (func, e1, e2)

lista_a_conj :: [Z] -> Expr
lista_a_conj = foldr (Union . Unit) Vacio 

conj1 = lista_a_conj [1, 2, 3]
conj2 = lista_a_conj [2, 3, 4]
conj3 = Union conj1 conj2 
conj4 = Interseccion conj1 conj2

pert1 = Pertenencia 2 conj1
pert2 = Pertenencia 3 conj4

incl1 = Inclusion conj1 conj2 
incl2 = Inclusion conj4 conj2 
incl3 = Inclusion conj1 conj3

ass1 = Asignacion "w" conj1 
ass2 = Asignacion "x" conj4 
ass3 = Asignacion "y" pert2 
ass4 = Asignacion "z" incl2

prettify :: Expr -> String 
prettify (Var x) = x
prettify (Asignacion x e) = (x ++ " := ") ++ prettify e
prettify Vacio = "[]"
prettify (Unit z) = show z
prettify (Pertenencia z e) = show z ++ " ∈ " ++ "[" ++ prettify e ++ "]"
prettify (Union e1 e2) = prettify e1 ++ " ∪ " ++ prettify e2 
prettify (Interseccion e1 e2) = "[" ++ prettify e1 ++ "]" ++ " ∩ " ++ "[" ++ prettify e2 ++ "]" 
prettify (Diferencia e1 e2) = "[" ++ prettify e1 ++ "]" ++ " - " ++ "[" ++ prettify e2 ++ "]" 
prettify (Inclusion e1 e2) = "[" ++ prettify e1 ++ "]" ++ " ⊆ " ++ "[" ++ prettify e2 ++ "]" 

print_expr :: Expr -> IO () 
print_expr = putStrLn . prettify


import Test.HUnit
