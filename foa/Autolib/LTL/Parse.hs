module Autolib.LTL.Parse where

import Autolib.LTL.Data
import Autolib.LTL.Print
import Autolib.Reader

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

instance Reader Formula where reader = formula

formula :: Parser Formula
formula = 
   let op f = Infix $ do 
           my_symbol (show f) ; return $ Binary f
   in  buildExpressionParser
    [ [ op And AssocLeft ]
    , [ op Or AssocLeft ]
    , [ op Until AssocNone 
      , op Implies AssocNone 
      , op Iff AssocNone 
      ]
    ] application

application = do
    us <- many unary
    a <- atom
    return $ foldr Unary a us

unary = foldr1 ( <|> ) $ do
   u <- uops
   return $ do my_reserved ( show u ) ; return u

atom = my_parens formula 
    <|> do v <- reader ; return $ Variable v

instance Reader Name where
    reader = do 
        n <- my_identifier
        guard $ not $ elem n 
              $ map show uops ++ map show bops
        return $ Name n

