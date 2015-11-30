import Parser
import Draw

import Control.Applicative ((<$>))
import Control.Monad ((>>=))

-----------------------------Main File----------------------------------
------------------------------------------------------------------------

-- Generates aux.tex that has to be compiled.

main = parseExp <$> getLine >>= writeCircuit
