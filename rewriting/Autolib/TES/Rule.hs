{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

module Autolib.TES.Rule where

--   $Id$

import Autolib.Symbol
import Autolib.TES.Term

import Autolib.ToDoc
import Autolib.Reader

import Autolib.TES.Parsec

type Rule v c = ( Term v c, Term v c )

instance Reader ( Term v c ) => Reader ( Rule v c ) where
    readerPrec p = do
        lhs <- readerPrec 0
	reservedOp trs "->"
	rhs <- readerPrec 0
	return ( lhs, rhs )

instance ToDoc ( Term v c ) => ToDoc ( Rule v c ) where
    toDoc (lhs, rhs) = hsep [ toDoc lhs, text "->", toDoc rhs ]


    

