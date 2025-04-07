{-# OPTIONS_GHC -fno-warn-tabs #-}

module Chi where 

import LookupTable

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
efectoS s (Case e bs)  = Case (efectoS s e) $ map (efectoSRama s) bs   

