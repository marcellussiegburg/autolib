module Reporter.Boolean.Reader where

--  $Id$

import Reporter.Boolean.Data
import Reader

import Text.ParserCombinators.Parsec.Expr

instance Reader i => Reader (Boolean i) where readerPrec p = expression
instance Reader i => Read (Boolean i) where readsPrec = parsec_readsPrec

expression :: Reader i 
	   => Parser ( Boolean i )
expression = buildExpressionParser operators atomic

atomic :: Reader i
       => Parser ( Boolean i )
atomic =   my_parens expression
       <|> do my_reserved "not" ; a <- atomic ; return $ Not a
       <|> do a <- reader ; return $ Atomic a
       <?> "atomic expression"

operators = do
    op <- reverse [ minBound .. maxBound ] -- largest predecence first
    return [ Infix ( do { my_reserved $ name op ; return $ bin op }
                 <?> "operator" ) AssocLeft
	   ]
    
