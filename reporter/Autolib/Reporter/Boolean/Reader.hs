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

operators = 
    [ [ op "and" bin_and AssocLeft ]
    , [ op "or" bin_or AssocLeft ]
    ]
    where
      op name f assoc   =
         Infix ( do { my_reserved name; return f }
                 <?> "operator" ) assoc
