module Reporter.Boolean.ToDoc where

--  $Id$

import Reporter.Boolean.Data
import ToDoc

instance Boolit i => ToDoc (Boolean i) where
    toDocPrec p ( Not x ) 
	= text "not" <+> toDocPrec 5 x
    toDocPrec p ( And xs ) 
	= docParen ( p > 3 ) 
	$ fsep $ punctuate ( text "and" ) $ map ( toDocPrec 3 ) xs
    toDocPrec p ( Or xs ) 
	= docParen ( p > 1 ) 
	$ fsep $ punctuate ( text "or" ) $ map ( toDocPrec 1 ) xs
    toDocPrec p ( Atomic a ) 
	= toDocPrec p a
