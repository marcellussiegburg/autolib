module TES.Symbol where

-- $Id$

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

type Signature = Set Symbol

instance ToDoc Symbol where 
    toDoc = text . name
instance Reader Symbol where 
    readerPrec p = do
        i <- identifier tes <|> fmap show ( natural tes )
	return $ Symbol { name = i , arity = error "undefined arity" }

instance Show Symbol where show = render . toDoc
instance Read Symbol where readsPrec = parsec_readsPrec
