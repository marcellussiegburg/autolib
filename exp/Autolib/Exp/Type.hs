module Exp.Type where

--  $Id$

data Exp 
	= Ref String  -- reference to value in evironment
	| Letter Char

	| Dot Exp Exp
	| Union Exp Exp
	| Intersection Exp Exp
	| Difference Exp Exp
	| SymDiff Exp Exp
	| Shuffle Exp Exp

	| Left_Quotient Exp Exp
	| Right_Quotient Exp Exp

	| Star Exp
	| Plus Exp
	| Power Integer Exp 
    deriving ( Eq, Ord )


