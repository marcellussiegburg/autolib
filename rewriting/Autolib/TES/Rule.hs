module TES.Rule where

--   $Id$

import TES.Symbol
import TES.Term

import ToDoc
import Reader

import Parsec
import ParsecToken
import TES.Parsec

type Rule v c = ( Term v c, Term v c )

instance Reader ( Term v c ) => Reader ( Rule v c ) where
    readerPrec p = do
        lhs <- readerPrec 0
	reservedOp tes "->"
	rhs <- readerPrec 0
	option () ( reservedOp tes ";" <|> reservedOp tes "." )
	return ( lhs, rhs )

instance Reader ( Term v c ) => Read ( Term v c ) where
    readsPrec = parsec_readsPrec

instance ToDoc ( Term v c ) => ToDoc ( Rule v c ) where
    toDoc (lhs, rhs) = hsep [ toDoc lhs, text "->", toDoc rhs ]

instance ToDoc ( Term v c ) => Show ( Rule v c ) where
    show = render . toDoc


