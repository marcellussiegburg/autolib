{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

module Autolib.TES.Rule where

--   $Id$

import Autolib.Symbol
import Autolib.TES.Term

import Autolib.ToDoc
import Autolib.Reader

import Autolib.TES.Parsec

type Rule v c = ( Term v c, Term v c )

instance ( Reader c, Reader ( Term v c ) ) => Reader ( Rule v c ) where
    reader = rule_reader
instance ( ToDoc c, ToDoc ( Term v c ) ) => ToDoc ( Rule v c ) where
    toDoc = rule_writer

rule_reader :: (  Reader t )
	    => Parser (t , t)
rule_reader = do
        lhs <- readerPrec 0
	reservedOp trs "->"
	rhs <- readerPrec 0
	return ( lhs, rhs )

rule_writer :: ToDoc t 
	    => (t,  t) -> Doc
rule_writer (lhs, rhs) = hsep [ toDoc lhs, text "->", toDoc rhs ]

    

