module TES.Identifier where

--   $Id$

import Sets

import ToDoc
import Reader

import TES.Parsec
import TES.Symbol


data Identifier = Identifier { name :: String
		     , i_arity :: Int
		     }
     deriving ( Eq, Ord )

instance Symbol Identifier where
     arity = i_arity

mknullary :: String -> Identifier
mknullary c = Identifier { name = c, i_arity = 0 }

mkunary :: String -> Identifier
mkunary c = Identifier { name = c, i_arity = 1 }

type Signature = Set Identifier

mkusig :: [ String ] -> Signature
-- make unary signature ( for string rewriting )
mkusig fs = mkSet $ do
    f <- fs
    return $ mkunary f

instance ToDoc Identifier where 
    toDoc = text . name
instance Reader Identifier where 
    readerPrec p = do
        i <- identifier tes 
	     <|> fmap show ( natural tes )
	     <|> operator tes
	return $ Identifier { name = i , i_arity = error "undefined arity" }

instance Show Identifier where show = render . toDoc
instance Read Identifier where readsPrec = parsec_readsPrec
