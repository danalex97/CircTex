module Circuit where

------------------------------------------------------------------------
------------------------------------------------------------------------

import Logics
import DNF

import Data.List
import Data.Maybe

import Control.Applicative
import Control.Monad

------------------------------------------------------------------------
------------------------------------------------------------------------

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

type GateInfo
  = (
      GateType , 
      (Maybe Float) , -- layer 
      (Maybe Float) , -- line
      (Maybe String) , -- input1
      (Maybe String)   -- input2
    )

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
   
------------------------------------------------------------------------
------------------------------------------------------------------------ 
  
instance Num a => Num (Maybe a) where
  (+)         = liftA2 (+)  
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  negate      = fmap negate
  fromInteger = Just . fromInteger
  
------------------------------------------------------------------------
------------------------------------------------------------------------
   
    
makeGate :: String -> GateType -> Gate
makeGate s t = ( s , (t , Nothing, Nothing, Nothing, Nothing) )

searchGate :: String -> [Gate] -> Maybe Gate
searchGate str gates 
  = lookup str ziped
  where
    ziped = zip (map fst gates) gates
  
sortedExps :: Exp -> [Exp]
--helper for labelGates
sortedExps = sort . getSub
 
labelGates :: [Exp] -> [Gate] -> [Gate]
-- Pre: exps are sorted
labelGates [] gates 
  = gates
labelGates (e:exps) gates
  = case e of 
      BinApp ap _ _ -> labelGates exps ( addGate t gates )
        where
          t = fromJust ( lookup ap mapGateType )  
      Not _         -> labelGates exps ( addGate I gates ) 
      Id s          -> labelGates exps ( makeGate s S : gates )     
 
addGate :: GateType -> [Gate] -> [Gate]
-- adds a gate of type t to the gate vector
addGate t gates
  = gate : gates
  where
    gate = makeGate gateName t
    tt   = show t
    
    gateName 
      = head $ dropWhile (\t -> searchGate t gates /= Nothing ) names 
    names    
      = map (tt ++) ( "" : map show [2,3..])
    
mapGates :: Exp -> [(Exp, Gate)]
-- map each expression with a gate
mapGates exp
  = zip exps gates
  where 
    gates = reverse $ labelGates exps [] 
    exps  = sortedExps exp
    
addInputs :: Gate -> Maybe String -> Maybe String -> Maybe Float -> Gate
-- assignes the inputs and the layer
addInputs ( s , (t,x,y,i1,i2) ) ni1 ni2 nx
  =  ( s , (t,nx,y,ni1,ni2) )

addLine :: Gate -> Maybe Float -> Gate
-- assignes the line to the gate
addLine ( s , (t,x,y,i1,i2) ) ny
  =  ( s , (t,x,ny,i1,i2) )

getLayer :: Gate -> Maybe Float
getLayer ( _ , (_,x,_,_,_) )
  = x
  
getGateType :: Gate -> GateType
getGateType ( _ , (x,_,_,_,_) )
  = x
    
getInputs :: [(Exp, Gate)] -> [(Exp, Gate)] -> [(Exp, Gate)]
-- pre: exps are sorted
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
              l1 = getLayer a1
              l2 = getLayer a2
          ---------
          Not e1 -> (exp, addInputs gate j1 Nothing l1)
            where
              a1@(g1, i1) = fromJust ( lookup e1 lst' )
              
              l1 = getLayer a1 + Just 1
              j1 = Just g1        
          ---------
          _  -> (exp, addInputs gate Nothing Nothing (Just 1))
    
    lst' = (l:lst) ++ sn 
    
getLines :: [(Exp, Gate)] -> [(Exp, Gate)]
-- generates the lines for all gates
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
      = length ( filter (\y -> getLayer x == getLayer y ) ls ) + 1

widenCircuit :: [Gate] -> [Gate]
-- widens the circuit with constant 0.5
widenCircuit gates 
  = map (\g -> changeLayer g ( layers - gLayer g ) ) gates
  where
    gLayer :: Gate -> Float
    gLayer g
      = ( fromJust . getLayer ) g 
    layers  
      = ( fromJust . maximum ) ( map getLayer gates )
    changeLayer ( s , (t,x,y,i1,i2) ) times
      = ( s , (t,nx,ny,i1,i2) )
      where
        nx = x * Just 2
        ny = y * Just (times+1) * Just 0.5 - Just 5
    
getCircuit :: Exp -> [Gate]
-- gets the information of the circuit out of an expression
getCircuit exp = ( snd . unzip . getLines ) mapExpGates
  where
    mapExpGates = getInputs ( mapGates . eachTwoDNF $ exp ) []

