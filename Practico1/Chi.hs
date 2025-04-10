{-# OPTIONS_GHC -fno-warn-tabs #-}

module Chi where 

import LookupTable
import Distribution.Simple.Program.HcPkg (list)

type K = String --Constructor 
type X = String --Variable 

data E = Var X 
    | Cons K [E] 
    | Lambda X E
    | Ap E E 
    | Case E [B]
    | Rec X E 
    deriving Show
data B = Rama K [X] E 
    deriving Show

data V = ConsV K [V] 
    | LambdaV X E 
    deriving Show

data W = ConsW K [E]
    | LambdaW X E
    deriving Show

type S = Table K E 

buscarRama :: K -> [B] -> Maybe B 
buscarRama k = find (\Rama k' es' -> k == k') 

efectoSRama :: S -> B -> B 
efectoSRama s (Rama k xs e) = let s' = foldr del s xs in
    Rama k xs $ efectoS s' e

efectoS :: S -> E -> E  
efectoS s (Var x) = case lkup x s of 
    Nothing -> error "Variable libre" 
    Just e -> e
efectoS s (Cons k es) = Cons k $ map (efectoS s) es 
efectoS s (Lambda x e) = Lambda x $ efectoS (del x s) e
efectoS s (Ap e1 e2) = Ap (efectoS s e1) (efectoS s e2)
efectoS s (Case e bs) = Case (efectoS s e) $ map (efectoSRama s) bs   
efectoS s (Rec x e) = Rec x $ efectoS (del x s) e 

listsToTuples :: [a] -> [b] -> [(a, b)]
listsToTuples [] ys = [] 
listsToTuples xs [] = [] 
listsToTuples (x:xs) (y:ys) = (x, y) : listsToTuples xs ys 

evalParcial :: E -> W 
evalParcial (Cons k es) = ConsW k es 
evalParcial (Lambda x e) = LambdaW x e
evalParcial (Ap e e') = case evalParcial e of 
    LambdaW x e'' -> evalParcial $ efectoS [(x, e')] e''  
    ConsW k es -> ConsW k (es ++ [e'])
evalParcial (Case e bs) = case evalParcial e of 
    LambdaW _ _ -> error "Expresion del case debe evaluarse a un constructor" 
    ConsW k es -> case buscarRama k bs of 
        Just (Rama k' xs e') -> assert (length es == length xs) $
            evalParcial $ efectoS (listsToTuples xs es) e'
        Nothing -> error "No se logro cubrir el caso para el constructor: " ++ k
evalParcial (Rec x e) = evalParcial $ efectoS [(x, Rec x e)] e 
