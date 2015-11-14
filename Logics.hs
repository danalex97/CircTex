module Logics where 

import Data.Maybe 
import Data.List 

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
  deriving (Eq)

depth :: Exp -> Int
depth exp 
  = case exp of 
      Val _ -> 0
      Id  _ -> 1
      Not e -> depth e + 1
      BinApp op e1 e2 -> max (depth e1) (depth e2) + 1

instance Ord Exp where
  x < y  = depth x < depth y
  x <= y = depth x <= depth y

type Env = [(String, State)]

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

instance Num State where
  F + F = F
  T + F = T
  F + T = T
  T + T = T
  
  _ + U = U 
  U + _ = U

  T * T = T
  T * F = F
  F * T = F
  F * F = F

  _ * U = U
  U * _ = U 

  negate      = undefined
  fromInteger = toState
  abs         = undefined
  signum      = undefined  
 
class LogicOp a where
  (^.) , (^/) , (+/) , (*/)  :: a -> a -> a  
  (!) :: a -> a

instance LogicOp Exp where
  (^.)  = BinApp Xor
  (^/)  = BinApp Xnor
  (+/)  = BinApp Nor
  (*/)  = BinApp Nand
  (!) x = Not x

instance LogicOp State where
  U ^. _ = U 
  _ ^. U = U
  T ^. F = T
  F ^. T = F
  _ ^. _ = F
  x ^/ y = (!) ( x ^. y )
  (!) T  = F
  (!) F  = T
  (!) U  = U
  x +/ y = (!) ( x + y )
  x */ y = (!) ( x * y )

binApps 
  = [ (Xor, "^.") 
    , (Or, "+")
    , (And, "*")
    , (Nor, "*/")
    , (Nand, "+/")
    , (Xnor, "^/")
    ]

binFunc 
  = [ (Xor, (^.) )
    , (Xnor, (^/) )
    , (Or, (+) )
    , (Nor, (+/) )
    , (And, (*) ) 
    , (Nand, (*/) )
    ]
    

showExp :: Exp -> String
showExp (BinApp op a b) = " (" ++ showExp a ++ fromJust ( lookup op binApps ) ++ showExp b  ++ ") "
showExp (Val a)         = show a 
showExp (Id a)          = a
showExp (Not a)         = "(!)" ++ showExp a

instance Show Exp where
  show = showExp

evalExp :: Exp -> Env -> State
evalExp (BinApp op a b) env = fromJust ( lookup op binFunc ) (evalExp a env) (evalExp b env)
evalExp (Not a) env         = (!) (evalExp a env)
evalExp (Id a) env          = fromJust ( lookup a env )
evalExp (Val a) _           = a 
	
getSub :: Exp -> [Exp]
-- gets all the subexpression of a logic expression
getSub (Not exp)
  = nub ( Not exp : getSub exp )
getSub exp@(BinApp op e1 e2)
  = nub ( exp : getSub e1 ++ getSub e2 )
getSub x
  = [x]
 
