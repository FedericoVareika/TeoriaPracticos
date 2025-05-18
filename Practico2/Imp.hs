{-# OPTIONS_GHC -fno-warn-tabs #-}

module Imp where

import LookupTable

-- 1) Tipos de IMP

type C = String

type X = String

data Prog
  = Asig [X] [E]
  | Local [X] Prog
  | (:>) Prog Prog
  | Case X [B]
  | While X [B]

data B = Rama C [X] Prog

data E
  = Cons C [E]
  | Var X

data V
  = ConsV C [V]
  | Null
  deriving (Show, Eq)

-- 2) Memoria

type M = LookupTable.Tabla X V

alta :: M -> [X] -> M
alta = foldr (\x -> (:) (x, Null))

-- 3) Evaluacion de expresiones

eval :: M -> E -> V
eval m (Cons c es) = ConsV c (map (eval m) es)
eval m (Var x) = case lkup x m of
  Just v -> v
  Nothing -> error "No se pudo encontrar la variable en memoria"

-- 4) Ejecucion del programa

promote :: V -> E 
promote (ConsV c vs) = Cons c (map promote vs) 

execute :: Prog -> M -> M
execute (Asig xs es) m = update m (zip xs vs) 
  where
    vs = map (eval m) es
execute (Local xs p) m = bajas (execute p $ alta m xs) xs
execute (p1 :> p2) m = execute p2 $ execute p1 m
execute (Case x bs) m =
  let (ConsV c vs) = eval m (Var x)
      (xs, p) = case buscarRama c bs of
        Just (Rama c' xs' p') -> (xs', p')
        Nothing -> error "Case no encontro rama coincidente" 
      newprog = Local xs $ Asig xs (map promote vs) :> p  
  in execute newprog m
execute (While x bs) m = 
  let (ConsV c vs) = eval m (Var x) 
  in case buscarRama c bs of 
    Just (Rama c' xs p) -> 
      let m' = execute (Local xs $ Asig xs (map promote vs) :> p) m
      in execute (While x bs) m' 
    Nothing -> m

buscarRama :: C -> [B] -> Maybe B
buscarRama c = find (\(Rama c' xs' p) -> c == c')

find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x : xs)
  | p x = Just x
  | otherwise = find p xs

-- 5) Programar en Imp 

-----Helpers------

notImp :: Prog
notImp = Case "value" [
        Rama "False" [] (Asig ["result"] [Cons "True" []]),
        Rama "True" [] (Asig ["result"] [Cons "False" []])
    ]

vN :: Int -> V
vN 0 = ConsV "Z" []
vN n = ConsV "S" [vN (n-1)]

------------------

---- par 
par :: Prog 
par = Asig ["result"] [Cons "True" []] :>  
    While "n" [
        Rama "S" ["n'"] (Asig ["value", "n"] [Var "result", Var "n'"] :> notImp)
    ]

parTests :: [(Prog, M, V)] 
parTests = [
        (par, [("n", vN 0)], ConsV "True" []),
        (par, [("n", vN 1)], ConsV "False" []),
        (par, [("n", vN 2)], ConsV "True" []),
        (par, [("n", vN 3)], ConsV "False" [])
    ]

validateTest :: (Prog, M, V) -> Bool 
validateTest (t, m, r) = case lkup "result" (execute t m) of 
    Nothing -> False
    Just r' -> r == r'

validateTests :: [(Prog, M, V)] -> Bool
validateTests = foldr (\test -> (&&) (validateTest test)) True

