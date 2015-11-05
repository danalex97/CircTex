module Logics where 

import qualified GHC.Real as GR

data BinOp 
  = And 
  | Or
  | Xor 
  | Nand
  | Nor
  | Xnor
  deriving (Eq, Ord, Show)

data State 
  = T
  | F 
  | U
  deriving (Eq, Ord, Show)

data Exp 
  = Val State
  | Id String
  | Not Exp
  | BinApp BinOp Exp Exp
  deriving (Eq, Ord)

toState :: (Eq a, Num a) => a -> State
toState 0 = F
toState 1 = T
toState _ = U

instance Num Exp where 
  (+)         = BinApp Or
  (*)         = BinApp And
  negate      = Not
  fromInteger = Val . toState
  abs         = undefined
  signum      = undefined 

infixl 5 ^.
(^.) :: Exp -> Exp -> Exp
(^.) = BinApp Xor

infixl 5 ^/
(^/) :: Exp -> Exp -> Exp
(^/) = BinApp Xnor

infixl 5 +/
(+/) :: Exp -> Exp -> Exp
(+/) = BinApp Nor

infixl 5 */ 
(*/) :: Exp -> Exp -> Exp
(*/) = BinApp Nand

-- not -- to add

showExp :: Exp -> String
showExp (UnApp Xor a b)  = " ( " ++ showExp a ++ "^." ++ showExp b  ++ " ) "
showExp (UnApp Xnor a b) = " ( " ++ showExp a ++ "^/" ++ showExp b  ++ " ) "
showExp (UnApp Or a b)   = " ( " ++ showExp a ++ "*" ++ showExp b  ++ " ) "
showExp (UnApp Nor a b)  = " ( " ++ showExp a ++ "*/" ++ showExp b  ++ " ) "
showExp (UnApp And a b)  = " ( " ++ showExp a ++ "+" ++ showExp b  ++ " ) "
showExp (UnApp Nand a b) = " ( " ++ showExp a ++ "+/" ++ showExp b  ++ " ) "
showExp (Val a)          = show a 
showExp Id               = id
showExp (Not a)          = showExp a ++ "'"

instance Show Exp where
  show = printExp
