module TES.Symbol where

-- -- $Id$

import Sets

import ToDoc
import Reader

import Parsec
import ParsecToken
import TES.Parsec


data Symbol = Symbol { name :: String
		     , arity :: Int
		     }
     deriving ( Eq, Ord )

mknullary :: String -> Symbol
mknullary c = Symbol { name = c, arity = 0 }

mkunary :: String -> Symbol
mkunary c = Symbol { name = c, arity = 1 }

type Signature = Set Symbol

mkusig :: [ String ] -> Signature
-- make unary signature ( for string rewriting )
mkusig fs = mkSet $ do
    f <- fs
    return $ mkunary f

instance ToDoc Symbol where 
    toDoc = text . name
instance Reader Symbol where 
    readerPrec p = do
        i <- identifier tes 
	     <|> fmap show ( natural tes )
	     <|> operator tes
	return $ Symbol { name = i , arity = error "undefined arity" }

instance Show Symbol where show = render . toDoc
instance Read Symbol where readsPrec = parsec_readsPrec
