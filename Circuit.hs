module Circuit where

--------------------Circuit Generation Module---------------------------
------------------------------------------------------------------------

-- This module implements circuit generation out of expressions.
-- The circuit will generated in LaTeX in "Draw" module.

import Logics
import DNF

import Data.List
import Data.Maybe

import Control.Applicative
import Control.Monad

--------------------------Class Definitions-----------------------------
------------------------------------------------------------------------

-- We defined each gate type with a unique signature.
-- For binary input gates look in mapGateType.
-- I stands for invertor and S stands for source.

data GateType 
  = X 
  | O 
  | A 
  | I 
  | Na 
  | No 
  | Xn 
  | S
  deriving (Show, Eq, Ord)

-- We dispose gates in "layers". 
-- A layer is defined by a nubed row in the logic exp. operation tree. 

-- Note the imput size might be 0,1 or 2. 
-- !! Warning: The modules do not support 3 or more input gates.

type GateInfo
  = (
      GateType , 
      (Maybe Float) ,  -- layer 
      (Maybe Float) ,  -- line
      (Maybe String) , -- input1
      (Maybe String)   -- input2
    )

-- A gate comes with a name.

type Gate 
  = ( String , 
      GateInfo
    )

mapGateType :: [(BinOp, GateType)]
mapGateType 
  = [ (Or, O) 
    , (And, A)
    , (Xor, X)
    , (Nor, No)
    , (Xnor, Xn)
    , (Nand, Na)
    ] 
   
--------------------------Maybe Helper Instatiation---------------------
------------------------------------------------------------------------ 
  
instance Num a => Num (Maybe a) where
  (+)         = liftA2 (+)  
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  negate      = fmap negate
  fromInteger = Just . fromInteger
  
---------------------------Helper functions-----------------------------
------------------------------------------------------------------------

-- Note some helper functions might be useful in other modules.
-- Marked with (*).

makeGate :: String -> GateType -> Gate
makeGate s t 
  = ( s , (t , Nothing, Nothing, Nothing, Nothing) )

searchGate :: String -> [Gate] -> Maybe Gate
-- Finds a gate by id. (*)
searchGate str gates 
  = lookup str ziped
  where
    ziped = zip (map fst gates) gates
     
mapGates :: Exp -> [(Exp, Gate)]
-- map each expression with a gate (*)
mapGates exp
  = zip exps gates
  where 
    gates = reverse $ labelGates exps [] 
    exps  = sortedExps exp
    
addInputs :: Gate -> Maybe String -> Maybe String -> Maybe Float -> Gate
-- assignes the inputs and the layer (*)
addInputs ( s , (t,x,y,i1,i2) ) ni1 ni2 nx
  =  ( s , (t,nx,y,ni1,ni2) )

addLine :: Gate -> Maybe Float -> Gate
-- assignes the line to the gate (*)
addLine ( s , (t,x,y,i1,i2) ) ny
  =  ( s , (t,x,ny,i1,i2) )

getMLayer :: Gate -> Maybe Float 
-- (*)
getMLayer ( _ , (_,x,_,_,_) )
  = x
  
getGateType :: Gate -> GateType
-- (*)
getGateType ( _ , (x,_,_,_,_) )
  = x
    
sortedExps :: Exp -> [Exp]
-- (*)
-- Instanciation to order is made into Logics module. Do not change it.
sortedExps 
  = sort . getSub
 
addGate :: GateType -> [Gate] -> [Gate]
-- adds a gate of type t to the gate vector (*)
addGate t gates
  = gate : gates
  where
    gate = makeGate gateName t
    tt   = show t
    
    gateName 
      = head $ dropWhile (\t -> searchGate t gates /= Nothing ) names 
    names    
      = map (tt ++) ( "" : map show [2,3..])

-------------------------Main function library--------------------------
------------------------------------------------------------------------

-- The transformations are made seqeuncially. 
-- Look at each component for further understanding.

labelGates :: [Exp] -> [Gate] -> [Gate]
-- Pre: Exps. are sorted.
labelGates [] gates 
  = gates
labelGates (e:exps) gates
  = case e of 
      BinApp ap _ _ -> labelGates exps ( addGate t gates )
        where
          t = fromJust ( lookup ap mapGateType )  
      Not _         -> labelGates exps ( addGate I gates ) 
      Id s          -> labelGates exps ( makeGate s S : gates )     

getInputs :: [(Exp, Gate)] -> [(Exp, Gate)] -> [(Exp, Gate)]
-- Pre: Exps. are sorted.
-- Generates inputs for all gates.
getInputs [] sn
  = reverse sn
getInputs (l:lst) sn
  = getInputs lst (modify l : sn)
  where
    modify (exp, gate)
      = case exp of 
          ---------
          BinApp ap e1 e2 -> (exp, addInputs gate j1 j2 maxLayer) 
            where
              maxLayer    = max l1 l2 + Just 1
              
              a1@(g1, i1) = fromJust ( lookup e1 lst' )
              a2@(g2, i2) = fromJust ( lookup e2 lst' )
              
              j1 = Just g1
              j2 = Just g2
              l1 = getMLayer a1
              l2 = getMLayer a2
          ---------
          Not e1 -> (exp, addInputs gate j1 Nothing l1)
            where
              a1@(g1, i1) = fromJust ( lookup e1 lst' )
              
              l1 = getMLayer a1 + Just 1
              j1 = Just g1        
          ---------
          _  -> (exp, addInputs gate Nothing Nothing (Just 1))
    
    lst' = (l:lst) ++ sn 
    
getLines :: [(Exp, Gate)] -> [(Exp, Gate)]
-- Generates the lines for all gates.
getLines allList
  = zip exps newLst
  where
    newLst 
    -- the new processed list
      = zipWith addLine lst ( Just <$> map fromIntegral ys )
    (exps, lst)
      = unzip allList
    ys         
    -- the list of values for the lines
      = map (\x -> solve x ( takeWhile (/=x) lst ) ) lst 
    solve x ls 
    -- helper function that counts the values on the same layer
      = length ( filter (\y -> getMLayer x == getMLayer y ) ls ) + 1

-------------------------Export function--------------------------------
------------------------------------------------------------------------

-- Transforms the expression into a circuit defined as a set of gates. 

getCircuit :: Exp -> [Gate]
getCircuit exp = ( snd . unzip . getLines ) mapExpGates
  where
    mapExpGates = getInputs ( mapGates exp ) []

