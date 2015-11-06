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

simpleDNF :: Exp -> Exp
-- gets a left associative DNF
simpleDNF exp
  = foldl1 (+) $ map ( foldl1 (*) ) minTerms
  where 
    minTerms = getMinterms exp

eachTwoWith :: Exp -> ( Exp -> Exp -> Exp ) -> Exp
-- groups a set of MinTerms at half with respect to an operation
eachTwoWith = undefined
