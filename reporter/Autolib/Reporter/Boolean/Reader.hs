module Reporter.Boolean.Reader where

--  $Id$

import Reporter.Boolean.Data
import Reader

import Text.ParserCombinators.Parsec.Expr

instance Reader i => Reader (Boolean i) where readerPrec p = expression
instance Reader i => Read (Boolean i) where readsPrec = parsec_readsPrec

expression :: Reader i 
	   => Parser ( Boolean i )
expression = buildExpressionParser binary_operators star

star :: Reader i
     => Parser ( Boolean i )
star = do
    a <- atomic
    f <- option id $ do my_reserved "^*" ; return $ Uf Star
    return $ f a

atomic :: Reader i
       => Parser ( Boolean i )
atomic =   my_parens expression
       <|> do unary_operators 
       <|> do a <- reader ; return $ Atomic a
       <?> "atomic expression"

unary_operators :: Reader i
		=> Parser ( Boolean i )
unary_operators = foldr1 (<|>) $ do
    up <- reverse [ minBound .. maxBound ] -- precedence doesn't matter
    return $ do
	 my_reserved $ uname up
	 arg <- atomic
	 return $ Uf up arg

binary_operators = do
    bop <- reverse [ minBound .. maxBound ] -- largest predecence first
    return [ Infix ( do { my_reserved $ bname bop ; return $ bin bop }
                 <?> "operator" ) AssocLeft
	   ]
    
