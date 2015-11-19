module Format where

------------------------------------------------------------------------
------------------------------------------------------------------------

import Logics
import Circuit hiding (addLine)

import Data.Maybe
import Data.List ((\\), sort)
import Prelude hiding (getLine)
import Debug.Trace (trace) 

------------------------------------------------------------------------
------------------------------------------------------------------------

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

------------------------------------------------------------------------
------------------------------------------------------------------------

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
    
------------------------------------------------------------------------
------------------------------------------------------------------------

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
-- also alters the sorting !
alterLayersBy [] _ _  
  = []
alterLayersBy gates spaceLayers spaceGates
  = fst ++ foldl process [] layers
  where
    process :: [Gate] -> [Gate] -> [Gate]
    process acc layer
      = ( alterLayerBy layer' spaceGates ) ++ acc
      -- = layer' ++ acc
      where 
        layer' = i ++ [l']
        
        i  = init layer 
        l  = last layer
        l' = assLayer l (rightmost + spaceLayers)  
        
        rightmost
          | null acc  = 0
          | otherwise = (getLayer . head) acc
      
    (fst : layers) = map (\x -> filter ( (== x) . getLayer ) gates ) [1..nrLayers]
    nrLayers       = maximum ( map getLayer gates )  
  
------------------------------------------------------------------------
------------------------------------------------------------------------

highCircuit :: [Gate] -> [Gate]
highCircuit gates 
  = highCircuitBy gates 0.5
  
widthCircuit gates 
  = widthCircuitBy gates 2

moveSources :: [Gate] -> [Gate] 
moveSources gates 
  = moveSourcesBy gates 0

alterLayers :: [Gate] -> [Gate]
alterLayers gates 
  = alterLayersBy gates 2 1

formatCircuit :: [Gate] -> [Gate]
formatCircuit = moveSources 
             . alterLayers 
             . highCircuit

------------------------------------------------------------------------
------------------------------------------------------------------------

