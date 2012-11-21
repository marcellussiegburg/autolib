{-# LANGUAGE TemplateHaskell #-}

module Autolib.Exp.Type where

import Data.Typeable

-- | regular expressions
--
-- see 'Autolib.Exp.Read' for input syntax
--
-- see 'Autolib.Exp.Inter.std' and
-- see 'Autolib.Exp.Inter.std_sigma'
-- for pre-defined identifiers

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

	| Power Integer (RX c) 
	| PowerStar (RX c)
	| PowerPlus (RX c)
        | PowerOmega (RX c)
    deriving ( Eq, Ord, Typeable )

-- | for  backward compatibility
type Exp = RX Char 



    
    