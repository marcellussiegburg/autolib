module TES.Symbol where

--   $Id$

import ToDoc
import Reader
import Text.ParserCombinators.Parsec.Expr 

import SRS.Aged

import Data.Maybe ( isJust )

class ( Eq s, Ord s, ToDoc s, Show s, Reader s ) => Symbol s where
    arity :: s -> Int
    set_arity :: Int -> s -> s

    precedence :: s -> Maybe Int
    precedence _ = Nothing -- default: not as operator

    assoc :: s -> Assoc
    assoc _ = AssocNone

is_operator :: Symbol s => s -> Bool
is_operator = isJust . precedence 

is_constant :: Symbol s => s -> Bool
is_constant = (== 0) . arity

instance ( Show a, Symbol a ) => Symbol (Aged a) where
    arity = arity . it
    set_arity a = fmap (set_arity a)  


    
