module DNF where

import Logics
import Data.List
import Control.Monad
import Control.Applicative

type Context = [String]
type MinTerm = [Exp]
-- a context is composed of all the variables in a boolean expression

getContext :: Exp -> Context
getContext 
  = nub . getContext'
  where   
    getContext' ( BinApp op a b ) = getContext a ++ getContext b
    getContext' ( Not a )         = getContext a
    getContext' ( Val a )         = []
    getContext' ( Id  a )         = [a] 

binaries :: [[State]]
binaries = [] : [ new : old | old <- binaries, new <- [F,T] ]

confs :: Int -> [[State]]
confs n 
  = dropWhile (\x -> length x < n) $ takeWhile (\x -> length x <= n) binaries

getEnvs :: Exp -> [Env]
getEnvs exp 
  = zipWith zip ( repeat context ) ( confs l )
  where 
    context = getContext exp
    l       = length context

buildMinTerm :: Env -> MinTerm
buildMinTerm = map (\(s,v) -> if v == T then (Id s) else (!) (Id s) )

getMinterms :: Exp -> [MinTerm]
getMinterms exp 
  = map buildMinTerm trueEnvs 
  where 
    trueEnvs = filter (\x -> evalExp exp x == T) envs
    envs     = getEnvs exp
 
expMinterms :: Exp -> [Exp]
-- transforms the minterms in exps
expMinterms exp
  = map ( foldl1 (*) ) minTerms
  where 
    minTerms = getMinterms exp

simpleDNF :: Exp -> Exp
-- gets a left associative DNF
simpleDNF exp
  = foldl1 (+) ( expMinterms exp ) 

eachTwoWith :: [Exp] -> ( Exp -> Exp -> Exp ) -> Exp
-- groups a set of expessions at half with respect to an operation
eachTwoWith exps op 
  = case l of
      1 -> exp
        where 
          [exp] = exps
      2 -> op e1 e2
        where 
          [e1, e2] = exps
      _ -> op (eachTwoWith lt op) (eachTwoWith rt op)
        where
          (lt, rt) = splitAt (l `div` 2) exps
  where
    l = length exps

eachTwoDNF :: Exp -> Exp
eachTwoDNF exp
  = eachTwoWith expTerms (+)  
  where 
    expTerms = expMinterms exp 
