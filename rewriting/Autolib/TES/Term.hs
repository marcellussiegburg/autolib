module TES.Term 

( module TES.Term
, module Tree
)

where

-- $Id$

import TES.Symbol

import Sets
import Tree

import ToDoc
import Reader

import Parsec
import ParsecToken
import TES.Parsec

type Term = Tree Symbol

instance ToDoc Term where
    toDoc ( Node t xs ) = toDoc t <> 
	if null xs 
	then ToDoc.empty 
	else ToDoc.parens $ hcat $ punctuate ToDoc.comma $ map toDoc xs
instance Show Term where show = render . toDoc

instance Reader Term where
    readerPrec p = do
        t <- readerPrec 0 -- symbol
	xs <- option [] $ ParsecToken.parens tes
		        $ commaSep tes 
	                $ readerPrec 0 
	return $ Node ( t { arity = length xs } ) xs
instance Read Term where
    readsPrec = parsec_readsPrec

