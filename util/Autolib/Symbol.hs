module TES.Symbol where

--   $Id$

import ToDoc
import Reader
import Text.ParserCombinators.Parsec.Expr 

import SRS.Aged

class ( Eq s, Ord s, ToDoc s, Show s, Reader s ) => Symbol s where
    arity :: s -> Int
    set_arity :: Int -> s -> s
    -- | Nothing : as prefix
    -- Just p : binary: infix with precedence p
    --          unary: prefix possibly without parens
    precedence :: s -> Maybe Int
    precedence _ = Nothing -- default: do not use infix
    assoc :: s -> Assoc
    assoc _ = AssocNone

instance ( Show a, Symbol a ) => Symbol (Aged a) where
    arity = arity . it
    set_arity a = fmap (set_arity a)  


    
