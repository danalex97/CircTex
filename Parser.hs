module Parser where

import System.IO
import Control.Monad
import Logics

-- to install: cabal install parsec 

import Control.Applicative ((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

def = emptyDef { identStart      = letter
               , identLetter     = alphaNum
               , opStart         = oneOf "!^+*"
               , opLetter        = oneOf "!^+*"
               , reservedOpNames = [ "!" , "+" , "*" , "^." , "^/" , "+/" , "*/" ]
               , reservedNames   = [ "T" , "F" , "U" ]
               }
                          
TokenParser 
  { parens     = m_parens
  , identifier = m_identifier
  , reservedOp = m_reservedOp
  , reserved   = m_reserved
  , whiteSpace = m_whiteSpace
  } 
  = makeTokenParser def
            
parser :: Parser Exp
parser = buildExpressionParser table term <?> "expression"

table = [ [ Prefix (m_reservedOp "!"  >> return (Not) ) ]
        , [ Infix  (m_reservedOp "*"  >> return (BinApp And) ) AssocLeft]
        , [ Infix  (m_reservedOp "+"  >> return (BinApp Or ) ) AssocLeft]
        , [ Infix  (m_reservedOp "*/" >> return (BinApp Nand) ) AssocLeft]
        , [ Infix  (m_reservedOp "+/" >> return (BinApp Nor ) ) AssocLeft]
        , [ Infix  (m_reservedOp "^." >> return (BinApp Xor ) ) AssocLeft]
        , [ Infix  (m_reservedOp "^/" >> return (BinApp Xnor) ) AssocLeft]
        ]
        
term = m_parens parser 
       <|> fmap Id m_identifier
       <|> (m_reserved "T" >> return (Val T) )
       <|> (m_reserved "F" >> return (Val F) )
       <|> (m_reserved "U" >> return (Val U) )

parseExp :: String -> Exp
parseExp inp = 
  case parse parser "" inp of
     Left  err -> error "parse error in Parser.hs"
     Right ans -> ans     
