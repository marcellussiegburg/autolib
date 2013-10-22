{-# language OverlappingInstances, IncoherentInstances #-}

module Autolib.SRS.Rule where

import Autolib.Symbol
import Autolib.TES.Parsec

import Autolib.ToDoc
import Autolib.Reader

data Rule c = Rule { lhs :: [c], rhs :: [c] }

instance ( Symbol c ) => Reader ( Rule c ) where
    readerPrec p = do
        lhs <- many $ readerPrec 0
	reservedOp trs "->"
	rhs <- many $ readerPrec 0
	option "" $ my_comma -- use maximum munch
	return $ Rule lhs rhs 

instance Symbol c => ToDoc ( Rule c ) where
    toDoc u = hsep 
        [ toDoc_list $ lhs u
	, text "->"
	, toDoc_list $ rhs u
       	-- TODO: , ToDoc.comma 
	]

