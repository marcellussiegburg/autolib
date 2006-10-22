{-# OPTIONS -fallow-incoherent-instances #-}

module Autolib.Symbol where

--   $Id$

import Autolib.ToDoc
import Autolib.Reader
import Text.ParserCombinators.Parsec.Expr 

import Data.Maybe ( isJust )
import Autolib.Set
import Autolib.Hash
import Autolib.Util.Size

import GHC.Int ( Int16 )

class ( Size s, Hash s, Eq s, Ord s, ToDoc s, Show s, Reader s ) 
      => Symbol s where
    arity :: s -> Int
    arity s = 1

    set_arity :: Int -> s -> s
    set_arity a s = error "Symbol.set_arity undefined"

    -- | default: not as operator, i. e. prefix notation with parentheses
    -- if @Just p@, then for binary ops: use infix notation;
    -- for unary ops, use prefix notation without parentheses
    -- (allows  "! ! x")
    precedence :: s -> Maybe Int
    precedence _ = Nothing 

    assoc :: s -> Assoc
    assoc _ = AssocNone

    pool :: [ s ]
    pool = error "Symbol.pool undefined"

    symbol_toDoc :: s -> Doc
    symbol_toDoc = toDoc

    symbol_reader :: Parser s
    symbol_reader = reader

    -- | use to create a directory name
    stringify :: [ s ] -> String
    stringify = error "Symbol.stringify not implemented"

    -- | used for SRS output
    toDoc_list :: [ s ] -> Doc
    toDoc_list = hsep . map toDoc 


unused :: Symbol s
       => Int -> Set s -> [s]
unused n cs =
        let fs = filter ( \ c -> not $ c `elementOf` cs ) pool
        in  if length fs < n 
	    then error "no more unused symbols available"
	    else take n fs

-- | unary: omit parens around argument
-- binary: use infix notation
is_operator :: Symbol s => s -> Bool
is_operator s = is_binary_operator s || is_unary_operator s

is_binary_operator :: Symbol s => s -> Bool
is_binary_operator s = 
    ( 2 == arity s ) && isJust ( precedence s )

is_unary_operator  :: Symbol s => s -> Bool
is_unary_operator s = 
    ( 1 == arity s ) && isJust ( precedence s )

is_constant :: Symbol s => s -> Bool
is_constant = (== 0) . arity

------------------------------------------------------------------------

instance Symbol () 

{-
instance ( HeightC h, Show a, Symbol a ) => Symbol (Aged h a) where
    arity = arity . it
    set_arity a = itmap (set_arity a)  
-}

instance Symbol Char where
    arity c = 1
    symbol_toDoc = Autolib.ToDoc.char
    symbol_reader = alphaNum
    pool = [ '#', '%' ] ++ ['a' .. 'z' ] ++ ['A' .. 'Z' ] ++ [ '0' .. '9' ]
    stringify  = id
    toDoc_list = text

instance Symbol Int where
    pool = [ 0 .. ]

instance ( Symbol a, Symbol b ) => Symbol (a, b)

{-
instance ( Size h, HeightC h ) => Symbol h
-}






    
