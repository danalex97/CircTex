module DNF where

import Logics
import Data.List
import Control.Monad
import Control.Applicative

type Context = [String]
type MinTerm = [Exp]
-- a context is composed of all the variables in a boolean expression

getContext :: Exp -> Context
getContext = nub . getContext'
  where   
    getContext' ( BinApp op a b ) = getContext a ++ getContext b
    getContext' ( Not a )         = getContext a
    getContext' ( Val a )         = []
    getContext' ( Id  a )         = [a] 


