{-# OPTIONS_GHC -fno-warn-tabs #-}

module Imp where

import LookupTable
import Debug.Trace

-- 1) Tipos de IMP

type C = String

type X = String

type F = String

data Prog
  = Asig [X] [E]
  | Local [X] Prog
  | (:>) Prog Prog
  | Case X [B]
  | While X [B]
  | Def F [X] X Prog 
  | Run F [X] X 
  deriving (Show)

data B = Rama C [X] Prog
  deriving (Show)

data E
  = Cons C [E]
  | Var X
  deriving (Show)

data V
  = ConsV C [V]
  | Null
  deriving (Show, Eq)

-- 2) Memoria

type M = LookupTable.Tabla X V
type Env = LookupTable.Tabla F ([X], X, Prog)

alta :: M -> [X] -> M
alta = foldr (\x -> (:) (x, Null))

-- 3) Evaluacion de expresiones

eval :: M -> E -> V
eval m (Cons c es) = ConsV c (map (eval m) es)
eval m (Var x) = case lkup x m of
  Just v -> v
  Nothing -> error ("No se pudo encontrar la variable en memoria " ++ x)

-- 4) Ejecucion del programa

promote :: V -> E 
promote (ConsV c vs) = Cons c (map promote vs) 

execute :: Prog -> (Env, M) -> (Env, M)
execute (Asig xs es) (env, m) = (env, update m (zip xs vs))
  where
    vs = map (eval m) es
execute (Local xs p) (env, m) = 
  let (env', m') = execute p (env, alta m xs)
  in (env', bajas m' xs) 
execute (p1 :> p2) (env, m) = execute p2 (execute p1 (env, m))
execute (Case x bs) (env, m) =
  let (ConsV c vs) = eval m (Var x)
      (xs, p) = case buscarRama c bs of
        Just (Rama c' xs' p') -> (xs', p')
        Nothing -> error "Case no encontro rama coincidente" 
      newprog = Local xs $ Asig xs (map promote vs) :> p  
  in execute newprog (env, m)
execute (While x bs) (env, m) = 
  let (ConsV c vs) = eval m (Var x) 
  in case buscarRama c bs of 
    Just (Rama c' xs p) -> 
      let (env', m') = execute (Local xs $ Asig xs (map promote vs) :> p) (env, m)
      in execute (While x bs) (env', m') 
    Nothing -> (env, m)
execute (Def f params ret prog) (env, m) = 
  let env' = upd (f, (params, ret, prog)) env
  in (env', m)
execute (Run f params ret) (env, m) = 
  let (params', ret', prog) = case lkup f env of 
        Just v -> v 
        Nothing -> error "La funcion no esta definida"
      paramValues = map (\k -> fromJust (lkup k m)) params
      functionMem = zip params' paramValues
      (_, m') = execute prog (env, functionMem) 
      returnValue = case lkup ret' m' of 
        Just v -> v 
        Nothing -> Null
  in (env, upd (ret, returnValue) m) 

buscarRama :: C -> [B] -> Maybe B
buscarRama c = find (\(Rama c' xs' p) -> c == c')

find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x : xs)
  | p x = Just x
  | otherwise = find p xs

fromJust :: Maybe a -> a 
fromJust (Just a) = a 
fromJust Nothing = error "Trying to unwrap a nothing value" 

-- 5) Programar en Imp 

-----Unit Testing------

validateTest :: (Prog, M, V) -> Bool 
validateTest (t, m, r) = 
  let (_, m') = execute t ([], m)
      res = case lkup "result" m' of 
        Just r' -> r == r'
        Nothing -> False
  in case res of 
    False -> trace ("Test failed: " ++ (show m) ++ (show m')) False 
    True -> True

validateTests :: [(Prog, M, V)] -> Bool
validateTests = foldr (\test -> (&&) (validateTest test)) True

-----------------------

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

-- 7.e) AnyEven

anyEven :: Prog 
anyEven = 
  Def "not" ["val"] "ret" (
    Case "val" [
      Rama "True" [] (Asig ["ret"] [Cons "False" []]),
      Rama "False" [] (Asig ["ret"] [Cons "True" []])
    ]
  ) :>
  Def "even" ["n"] "ret" (
    Asig ["ret"] [Cons "True" []] :> 
    While "n" [
      Rama "S" ["n'"] (
        Run "not" ["ret"] "ret'" :>
        Asig ["ret", "n"] [Var "ret'", Var "n'"]
      )
    ]
  ) :>
  Def "or" ["v1", "v2"] "ret" (
    Case "v1" [
      Rama "True" [] (Asig ["ret"] [Cons "True" []]), 
      Rama "False" [] (
        Case "v2" [
          Rama "True" [] (Asig ["ret"] [Cons "True" []]), 
          Rama "False" [] (Asig ["ret"] [Cons "False" []]) 
        ]
      )
    ]
  ) :> 
  Run "even" ["x"] "resX" :>  
  Run "even" ["y"] "resY" :>  
  Run "even" ["z"] "resZ" :>  
  Run "or" ["resX", "resY"] "result" :> 
  Run "or" ["result", "resZ"] "result" 

anyEvenTests :: [(Prog, M, V)] 
anyEvenTests = [
    (anyEven, [("x", vN 0), ("y", vN 0), ("z", vN 0)], ConsV "True" []),
    (anyEven, [("x", vN 1), ("y", vN 0), ("z", vN 0)], ConsV "True" []),
    (anyEven, [("x", vN 0), ("y", vN 1), ("z", vN 0)], ConsV "True" []),
    (anyEven, [("x", vN 0), ("y", vN 0), ("z", vN 1)], ConsV "True" []),
    (anyEven, [("x", vN 1), ("y", vN 1), ("z", vN 0)], ConsV "True" []),
    (anyEven, [("x", vN 1), ("y", vN 0), ("z", vN 1)], ConsV "True" []),
    (anyEven, [("x", vN 0), ("y", vN 1), ("z", vN 1)], ConsV "True" []),
    (anyEven, [("x", vN 1), ("y", vN 1), ("z", vN 1)], ConsV "False" []),
    (anyEven, [("x", vN 2), ("y", vN 1), ("z", vN 1)], ConsV "True" []),
    (anyEven, [("x", vN 1), ("y", vN 2), ("z", vN 1)], ConsV "True" []),
    (anyEven, [("x", vN 1), ("y", vN 1), ("z", vN 2)], ConsV "True" []),
    (anyEven, [("x", vN 3), ("y", vN 1), ("z", vN 5)], ConsV "False" [])
  ]




