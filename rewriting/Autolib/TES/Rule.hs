module TES.Rule where

-- -- $Id$

import TES.Symbol
import TES.Term

import Tree

import ToDoc
import Reader

import Parsec
import ParsecToken
import TES.Parsec

type Rule = ( Term, Term )

instance Reader Rule where
    readerPrec p = do
        lhs <- readerPrec 0
	reservedOp tes "->"
	rhs <- readerPrec 0
	option () ( reservedOp tes ";" <|> reservedOp tes "." )
	return ( lhs, rhs )
instance Read Rule where
    readsPrec = parsec_readsPrec

instance ToDoc Rule where
    toDoc (lhs, rhs) = hsep [ toDoc lhs, text "->", toDoc rhs ]
instance Show Rule where
    show = render . toDoc


