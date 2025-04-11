{-# OPTIONS_GHC -fno-warn-tabs #-}

module Chi where

import Distribution.Simple.Program.HcPkg (list)
import LookupTable

type K = String -- Constructor

type X = String -- Variable

data E
  = Var X
  | Cons K [E]
  | Lambda X E
  | Ap E E
  | Case E [B]
  | Rec X E
  deriving (Show, Eq)

data B = Rama K [X] E
  deriving (Show, Eq)

data V
  = ConsV K [V]
  | LambdaV X E
  deriving (Show, Eq)

data W
  = ConsW K [E]
  | LambdaW X E
  deriving (Show, Eq)

type S = Table K E

find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x : xs)
  | p x = Just x
  | otherwise = find p xs

assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _ x = x

buscarRama :: K -> [B] -> Maybe B
buscarRama k = find (\(Rama k' xs' e) -> k == k')

efectoSRama :: S -> B -> B
efectoSRama s (Rama k xs e) =
  let s' = foldr del s xs
   in Rama k xs $ efectoS s' e

efectoS :: S -> E -> E
efectoS s (Var x) = case lkup x s of
  Nothing -> Var x
  Just e -> e
efectoS s (Cons k es) = Cons k $ map (efectoS s) es
efectoS s (Lambda x e) = Lambda x $ efectoS (del x s) e
efectoS s (Ap e1 e2) = Ap (efectoS s e1) (efectoS s e2)
efectoS s (Case e bs) = Case (efectoS s e) $ map (efectoSRama s) bs
efectoS s (Rec x e) = Rec x $ efectoS (del x s) e

listsToTuples :: [a] -> [b] -> [(a, b)]
listsToTuples [] ys = []
listsToTuples xs [] = []
listsToTuples (x : xs) (y : ys) = (x, y) : listsToTuples xs ys

evalParcial :: E -> W
evalParcial (Cons k es) = ConsW k es
evalParcial (Lambda x e) = LambdaW x e
evalParcial (Ap e e') = case evalParcial e of
  LambdaW x e'' -> evalParcial $ efectoS [(x, e')] e''
  ConsW k es -> ConsW k (es ++ [e'])
evalParcial (Case e bs) = case evalParcial e of
  LambdaW _ _ -> error "Expresion del case debe evaluarse a un constructor"
  ConsW k es -> case buscarRama k bs of
    Just (Rama k' xs e') ->
      assert (length es == length xs) $
        evalParcial $
          efectoS (listsToTuples xs es) e'
    Nothing -> error $ "No se logro cubrir el caso para el constructor: " ++ k
evalParcial (Rec x e) = evalParcial $ efectoS [(x, Rec x e)] e

eval :: E -> V 
eval e = case evalParcial e of 
    ConsW k es -> ConsV k $ map eval es
    LambdaW x e' -> LambdaV x e'

{-

7) Codificar en Chi

-}

-- OR --
chiOr :: E 
chiOr = Lambda "x" $ Lambda "y" $ Case (Var "x") [
        Rama "True" [] (Var "y"), 
        Rama "False" [] (Cons "False" []) 
    ]

testsChiOr :: [(E, V)]
testsChiOr = [
        (Ap (Ap chiOr $ Cons "False" []) $ Cons "False" [], ConsV "False" []),
        (Ap (Ap chiOr $ Cons "True" []) $ Cons "False" [], ConsV "False" []),
        (Ap (Ap chiOr $ Cons "False" []) $ Cons "True" [], ConsV "False" []),
        (Ap (Ap chiOr $ Cons "True" []) $ Cons "True" [], ConsV "True" [])
    ]

-- triple -- 
chiTriple :: E 
chiTriple = undefined

-- duplicar -- 
-- ramaC -- 
-- zeros -- 
-- takes -- 

    
validateTests :: [(E, V)] -> Bool
validateTests = foldr (\(t, r) -> (&&) (eval t == r)) True 
