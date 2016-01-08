module Parser where

------------------------------Parser Module-----------------------------
------------------------------------------------------------------------

import System.IO
import Control.Monad
import Logics

--- This module uses parsec and texlive is required for Main.sh

-- Install: cabal install parsec 
-- Install: sudo apt-get install texlive-full

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

------------------------------Helper Library----------------------------
------------------------------------------------------------------------

-- Operator precedence is given below in "table" from top to bottom.

def 
 = emptyDef 
 { identStart      = letter
 , identLetter     = alphaNum
 , opStart         = oneOf "!^+*"
 , opLetter        = oneOf "!^+*"
 , reservedOpNames = [ "!" , "+" , "*" , "^." , "^!" , "+!" , "*!" ]
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
parser 
  = buildExpressionParser table term <?> "expression"

table 
  = [ [ Prefix (m_reservedOp "!"  >> return (Not) ) ]
    , [ Infix  (m_reservedOp "*"  >> return (BinApp And) ) AssocLeft]
    , [ Infix  (m_reservedOp "+"  >> return (BinApp Or ) ) AssocLeft]
    , [ Infix  (m_reservedOp "*!" >> return (BinApp Nand) ) AssocLeft] -- issue -- to change in all files
    , [ Infix  (m_reservedOp "+!" >> return (BinApp Nor ) ) AssocLeft] -- issue -- to change in all files
    , [ Infix  (m_reservedOp "^." >> return (BinApp Xor ) ) AssocLeft]
    , [ Infix  (m_reservedOp "^!" >> return (BinApp Xnor) ) AssocLeft]
    ]
        
term 
  =  m_parens parser 
 <|> fmap Id m_identifier
 <|> (m_reserved "T" >> return (Val T) )
 <|> (m_reserved "F" >> return (Val F) )
 <|> (m_reserved "U" >> return (Val U) )

--------------------------Export Function-------------------------------
------------------------------------------------------------------------

parseExp :: String -> Exp
parseExp inp 
  = case parse parser "" inp of
      Left  err -> error "parse error in Parser.hs"
      Right ans -> ans     
