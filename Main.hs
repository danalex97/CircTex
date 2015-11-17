import Parser
import Draw

import Control.Applicative ((<$>))
import Control.Monad ((>>=))

main = parseExp <$> getLine >>= writeCircuit
