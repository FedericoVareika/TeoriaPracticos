{-# OPTIONS_GHC -fno-warn-tabs #-}

module TuringMachine where 

import LookupTable 

data Alphabet = Value String
  | Null

instance Show Alphabet where 
  show (Value x) = show x 
  show Null = "#"

alphabetLength :: Alphabet -> Int 
alphabetLength (Value x) = length x 
alphabetLength Null = 1

data State = Init
  | Halt 
  | Q String

data Action = Left 
  | Right 
  | Write Alphabet

type TuringMachine = Table State [Branch]
data Branch = Case Alphabet (Action, State)
  | Default (Action, State)

newtype Tape = Tape ([Alphabet], Alphabet, [Alphabet])

instance Show Tape where 
  show (Tape (l, c, r)) = 
    showSide True l  
    ++ " | " ++ show c ++ " | "
    ++ showSide False r  
    ++ "\n" 
    ++ replicate 
      (length (showSide True l) + 3 + divmod (alphabetLength c) 2)
      ' ' 
    ++ "^" 

divmod :: Int -> Int -> Int 
divmod x y = div x y + mod x y

showCursor :: Int -> String
showCursor n = replicate n ' ' ++ "^"

showSide :: Show a => Bool -> [a] -> String
showSide isLeft l = 
  let toShow 
        | isLeft = reverse l
        | otherwise = l 
  in showList toShow
  where 
    showList :: Show a => [a] -> String 
    showList (x:xs@(y:ys)) = show x ++ " | " ++ showList xs
    showList [x] = show x 
    showList [] = ""

createTape :: [String] -> String -> [String] -> Tape
createTape l c r = Tape (map Value (reverse l), Value c, map Value r)

-- execute :: (Tape, State) -> TuringMachine -> (Tape, State)
-- execute (

