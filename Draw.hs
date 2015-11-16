module Draw where

import Circuit
import Logics

import Data.Maybe

import Control.Applicative

--import System.Texrunner

top :: [String]
top 
  = [ "\\documentclass[border=3mm]{standalone} \n" 
    , "\\usepackage{tikz} \n" 
    , "\\usetikzlibrary{arrows,shapes.gates.logic.US,shapes.gates.logic.IEC,calc} \n" 
    , "\\begin{document} \n" 
    , "\\tikzstyle{branch}=[fill,shape=circle,minimum size=3pt,inner sep=0pt] \n" 
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
    , (I, "\\node[not gate US, draw]")
    , (S, "\\node")
    ]

drawGate :: Float -> Gate -> [String]
drawGate h (name, info)
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
          = [ "\\path (" ++ fromJust i1 ++ ") " 
            , " -- coordinate (punt" ++ fromJust i1 ++ ") " 
            , " (" ++ fromJust i1 ++ " |- " ++ name ++ ".input); \n" 
            ]
            ++
            [ "\\draw (punt" ++ fromJust i1 ++ ") "
            , "node[branch] {} -| "
            , " (" ++ name ++ ".input); \n" 
            ]
    (xx, yy)
      | t == S    = (show (y * 0.2), show ( 0.8 * h + 2 ) )
      | otherwise = (show x, show y)

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

drawCircuit :: [Gate] -> String
drawCircuit gates
  = concat all
  where
    all         = top ++ drawnGates ++ connections ++ bottom
    drawnGates  = concat ( map (drawGate h) gates )
    bigGates    = filter (\gt -> getGateType gt /= S && getGateType gt /= I ) gates
    h           = ( fromJust . maximum ) ( map getLayer gates )
    
    connections = concat ( map buildConn bigGates )
      where
        buildConn gt
          = drawConnections gt (takeGate i1) (takeGate i2)
          where
            (_, (_, _, _, Just i1, Just i2)) = gt
            takeGate name
              = getGateType $ (name, fromJust ( lookup name gates ) )
              
writeCircuit :: Exp -> IO ()          
writeCircuit exp 
  = writeFile "aux.tex" str
  where 
    str = (drawCircuit . widenCircuit . getCircuit) exp
    
main = read <$> getLine >>= writeCircuit

