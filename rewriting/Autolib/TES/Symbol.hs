module TES.Symbol where

--   $Id$

import ToDoc
import Reader
import Text.ParserCombinators.Parsec.Expr 

import SRS.Aged

import Data.Maybe ( isJust )
import Sets
import Hash
import Util.Size

import GHC.Int ( Int16 )

class ( Size s, Hash s, Eq s, Ord s, ToDoc s, Show s, Reader s ) 
      => Symbol s where
    arity :: s -> Int
    arity s = 1

    set_arity :: Int -> s -> s
    set_arity a s = error "Symbol.set_arity undefined"

    precedence :: s -> Maybe Int
    precedence _ = Nothing -- default: not as operator

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

is_operator :: Symbol s => s -> Bool
is_operator = isJust . precedence 

is_constant :: Symbol s => s -> Bool
is_constant = (== 0) . arity

------------------------------------------------------------------------

instance Symbol () where


instance ( Show a, Symbol a ) => Symbol (Aged a) where
    arity = arity . it
    set_arity a = itmap (set_arity a)  

instance Symbol Char where
    arity c = 1
    symbol_toDoc = ToDoc.char
    symbol_reader = alphaNum
    pool = [ '#', '%' ] ++ ['a' .. 'z' ] ++ ['A' .. 'Z' ] ++ [ '0' .. '9' ]
    stringify  = id
    toDoc_list = text

instance Symbol Int where
    pool = [ 0 .. ]








    
