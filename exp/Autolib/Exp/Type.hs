module Exp.Type where

--  $Id$

-- | regular expressions
data RX c
	= Ref String  -- ^ reference to value in evironment
	| Letter c

	| Dot (RX c) (RX c)
	| Union (RX c) (RX c)
	| Intersection (RX c) (RX c)
	| Difference (RX c) (RX c)
	| SymDiff (RX c) (RX c)
	| Shuffle (RX c) (RX c)

	| Left_Quotient (RX c) (RX c)
	| Right_Quotient (RX c) (RX c)

	| Star (RX c)
	| Plus (RX c)
	| Power Integer (RX c) 
    deriving ( Eq, Ord )

-- | for  backward compatibility
type Exp = RX Char 