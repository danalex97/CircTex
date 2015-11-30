module Format where

--------------------------Formating Module------------------------------
------------------------------------------------------------------------

-- Uses random for assigning the different lines.
-- Install: cabal install random

import Logics
import Circuit     hiding (addLine)

import Data.Maybe
import Data.List
import Prelude     hiding (getLine)
import Debug.Trace        (trace) 

import System.Random

----------Helper function for gate coordinate manipulation--------------
------------------------------------------------------------------------

-- (Line, Layer) will be the tuple used as coordinates in Tex file.

getMLine :: Gate -> Maybe Float
getMLine ( _ , (_,_,y,_,_) )
  = y

getLayer :: Gate -> Float
getLayer = fromJust . getMLayer

getLine :: Gate -> Float
getLine = fromJust . getMLine 	
 
gatesByLayer :: [Gate] -> Float -> [Gate]
gatesByLayer gates layer 
  = filter ( ( == layer ). getLayer ) gates
 
assLine :: Gate -> Float -> Gate
assLine ( s , (t,x,y,i1,i2) ) c
  = ( s , (t,x,Just c,i1,i2) )

assLayer :: Gate -> Float -> Gate
assLayer ( s , (t,x,y,i1,i2) ) c
  = ( s , (t,Just c,y,i1,i2) )
    
mulLine :: Gate -> Float -> Gate
mulLine g c
  = assLine g ( getLine g * c )

mulLayer :: Gate -> Float -> Gate
mulLayer g c
  = assLayer g ( getLayer g * c )
    
addLine :: Gate -> Float -> Gate
addLine g c
  = assLine g ( getLine g + c )

addLayer :: Gate -> Float -> Gate
addLayer g c
  = assLayer g ( getLayer g + c )
  
--------------------Randomized helper functions-------------------------
------------------------------------------------------------------------

randPermBy :: StdGen -> [a] -> [a]
randPermBy _ []   
  = []
randPermBy gen xs 
  = front : randPermBy newGen (take n xs ++ drop (n+1) xs)
  where
    (n,newGen) = randomR (0, length xs -1) gen
    front      = xs !! n

randPerm :: [a] -> [a]
randPerm 
  = randPermBy (mkStdGen 42)

----------------------Intermal function library-------------------------
------------------------------------------------------------------------

-- This functions are prototypes for the next section.
-- All of them receive scalable inputs.
-- Therefore, the formatting is open to user preferences. 

highCircuitBy :: [Gate] -> Float -> [Gate] 
highCircuitBy gates c
  = map (\g -> mulLine g ( ( layers - getLayer g ) * c )  ) gates
  where
    layers  
      = maximum $ map getLayer gates 

widthCircuitBy :: [Gate] -> Float -> [Gate]
widthCircuitBy gates c
  = map ((flip mulLayer) c) gates

moveSourcesBy :: [Gate] -> Float -> [Gate]
-- Sources will be displayed in upper side of the other gates.
moveSourcesBy gates height
  = newL ++ newT
  where
    l = gatesByLayer gates 1 
    t  = gates \\ l
        
    swap = \( s , (t,x,y,i1,i2) ) -> ( s , (t,y,x,i1,i2) )
    l'   = zipWith assLine l [ x * ct | x <- [1..] ]
    
    newL = map ((flip addLine) (height)) (map swap l')
    newT = map ((flip addLayer) width) t
    
    height = maximum ( map getLine t ) 
    width  = ( fromIntegral $ ( length l - 1 ) ) * ct
    
    ct = 0.5
    
----------------------Layer altering functions--------------------------
------------------------------------------------------------------------

-- Also scalable.
-- Layer altering make stairlike display for each layer.
-- For testing removing line altering is recommended.

alterLayerBy :: [Gate] -> Float -> [Gate]
alterLayerBy [] _
  = [] 
alterLayerBy [gate] _
  = [gate]
alterLayerBy (gate:gates) ct
  = ( assLayer gate (pl + ct) ) : nextGates
  where
    pl        = (getLayer . head) nextGates
    nextGates = alterLayerBy gates ct

alterLayersBy :: [Gate] -> Float -> Float -> [Gate]
-- Warning : The sorting will be removed!
alterLayersBy [] _ _  
  = []
alterLayersBy gates spaceLayers spaceGates
  = fst ++ foldl process [] layers
  where
    process :: [Gate] -> [Gate] -> [Gate]
    process acc layer
      = ( alterLayerBy layer' spaceGates ) ++ acc
      where 
        layer' = i ++ [l']
        
        i  = init layer 
        l  = last layer
        l' = assLayer l (rightmost + spaceLayers)  
        
        rightmost
          | null acc  = 0
          | otherwise = (getLayer . head) acc
      
    (fst : layers) = map filterLayers [1..nrLayers]
    filterLayers x = filter ( (== x) . getLayer ) gates
    nrLayers       = maximum ( map getLayer gates )  

---------------------------Main functions-------------------------------
------------------------------------------------------------------------

-- Main functions with given contstants.

highCircuit :: [Gate] -> [Gate]
highCircuit gates 
  = highCircuitBy gates 5
  
widthCircuit gates 
  = widthCircuitBy gates 2

moveSources :: [Gate] -> [Gate] 
moveSources gates 
  = moveSourcesBy gates 0

alterLayers :: [Gate] -> [Gate]
alterLayers gates 
  = alterLayersBy gates 2 1

alterLines :: [Gate] -> [Gate]
alterLines gates
  = zipWith assLine gates shuffle 
  where 
    n       = length gates
    shuffle = map fromIntegral ( randPerm [1..n] )

---------------------------Export function------------------------------
------------------------------------------------------------------------

-- Formats circuit in 4 stages:
-- * moves sources upside in the upper part of the document
-- * alter layers with stairlike display
-- * alter lines based on random line assigment with given seed
-- * widens the circuit vertically by given constant

formatCircuit :: [Gate] -> [Gate]
formatCircuit = moveSources 
             . alterLayers 
             . alterLines
             . highCircuit
