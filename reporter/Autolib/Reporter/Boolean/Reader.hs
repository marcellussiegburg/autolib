module Reporter.Boolean.Reader where

--  $Id$

import Reporter.Boolean.Data
import Reader

import Text.ParserCombinators.Parsec.Expr

instance Boolit i => Reader (Boolean i) where
    readerPrec p = expression

expression :: Boolit i 
	   => Parser ( Boolean i )
expression = buildExpressionParser operators atomic

atomic :: Boolit i
       => Parser ( Boolean i )
atomic =   my_parens expression
       <|> reader
       <?> "atomic expression"

operators = 
    [ [ op "and" bin_and AssocLeft ]
    , [ op "or" bin_or AssocLeft ]
    ]
    where
      op name f assoc   =
         Infix ( do { my_reserved name; return f }
                 <?> "operator" ) assoc
