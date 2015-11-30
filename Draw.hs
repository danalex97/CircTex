module Draw where

--------------------------Drawing Module--------------------------------
------------------------------------------------------------------------

-- Converts circuit to text file.

import Circuit
import Logics
import Format

import Data.Maybe

import Control.Applicative

--------------------Tex Settings and Gate Mappings----------------------
------------------------------------------------------------------------

top :: [String]
top 
  = [ "\\documentclass[border=3mm]{standalone} \n" 
    , "\\usepackage{tikz} \n" 
    , "\\usetikzlibrary{arrows,shapes.gates.logic.US,"
    , "shapes.gates.logic.IEC,calc} \n" 
    , "\\begin{document} \n" 
    , "\\tikzstyle{branch}="
    , "[fill,shape=circle,minimum size=3pt,inner sep=0pt] \n" 
    , "\\begin{tikzpicture}[label distance=2mm] \n"
    ]

bottom :: [String]
bottom 
  = [ "\\end{tikzpicture} \n" 
    , "\\end{document} \n"
    ]

drawMap :: [(GateType, String)]
drawMap 
  = [ (X, "\\node[xor gate US, draw, logic gate inputs=nn]") 
    , (O, "\\node[or gate US, draw, logic gate inputs=nn]") 
    , (A, "\\node[and gate US, draw, logic gate inputs=nn]") 
    , (Na, "\\node[nand gate US, draw, logic gate inputs=nn]") 
    , (No, "\\node[nor gate US, draw, logic gate inputs=nn]") 
    , (Xn, "\\node[xnor gate US, draw, logic gate inputs=nn]") 
    , (I,  "\\node[not gate US, draw]")
    , (S,  "\\node")
    ]
    
--------------------------Helper Functions------------------------------
------------------------------------------------------------------------

drawGate :: Gate -> [String]
drawGate (name, info)
  = [ fromJust (lookup t drawMap) 
    , " at (" ++ xx ++ ", " ++ yy ++ ") "
    , "(" ++ name ++ ") " 
    , "{" ++ name ++ "} ;\n"
    ]
    ++ 
    inverterConn
  where
    (t, Just x, Just y, i1, i2) 
      = info
    inverterConn 
      | t /= I    = []
      | otherwise 
          = [ "\\draw (" ++ fromJust i1 ++ ") "
            , "|- "
            , " (" ++ name ++ ".input); \n" 
            ]
    (xx, yy) = (show x, show y)

drawConnection :: String -> (String, GateType) -> Int -> String 
drawConnection name (i1,t1) input
  = "\\draw (" ++ fromGate ++ ") |- (" ++ toGate ++ ");"
  where
    fromGate 
      | t1 == S   = i1 
      | otherwise = i1 ++ ".output"
    toGate
      = name ++ ".input "++ show input 

drawConnections :: Gate -> GateType -> GateType -> [String]
drawConnections (name, info) t1 t2
  = [ drawConnection name (i1,t1) 1
    , drawConnection name (i2,t2) 2
    , "\n"
    ]
  where
    (t, _, _,Just i1,Just i2) = info
    
---------------------------Main Functions-------------------------------
------------------------------------------------------------------------

drawCircuit :: [Gate] -> String
-- Main function - generates the output text
drawCircuit gates
  = concat all
  where
    all        = top ++ drawnGates ++ connections ++ bottom
    drawnGates = concat ( map drawGate gates )
    bigGates   = filter hasBinInput gates 
      where
        hasBinInput gt 
          =  getGateType gt /= S 
          && getGateType gt /= I
    
    connections = concat ( map buildConn bigGates )
      where
        buildConn gt
          = drawConnections gt (takeGate i1) (takeGate i2)
          where
            (_, (_, _, _, Just i1, Just i2)) = gt
            takeGate name
              = getGateType $ (name, fromJust ( lookup name gates ) )
              
              
writeCircuit :: Exp -> IO ()        
-- Returns IO action for writting in the auxiliary Tex file.
writeCircuit exp 
  = writeFile "aux.tex" str
  where 
    str = (drawCircuit . formatCircuit . getCircuit) exp
